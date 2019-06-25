package console.parser

import java.io.File
import java.nio.file.Paths

import scala.sys.process.{Process, ProcessBuilder}
import scala.util.Properties

object Parser {
  sealed trait Error { def message: String }
  case class UnclosedQuote(message: String = "Unclosed quote in input") extends Error
  case class EmptyCommand(message: String = "Empty command due to badly placed operator") extends Error
  case class UnexpectedTrailingOperator(message: String = "Unexpected trailing operator") extends Error
  case class UndefinedVariable(message: String = "Undefined variable") extends Error
  case class MultipleErrors(errors: Seq[Error]) extends Error {
    val message: String = errors.map(_.message).mkString("\n")
  }
  case class HistoryExecNonZeroArguments(message: String =
                                         "Do not pass arguments to running commands from history") extends Error
  case class PathDoesNotExist(file: File) extends Error {
    val message: String = s"Path does not exist ${file.toString}"
  }
  case class InvalidSetArguments(message: String = "Command set takes exactly 2 arguments") extends Error

  def parse(tokens: Seq[Token]): Seq[Ast] = {
    def seqToCommand(command: Seq[Token]): Ast = {
      Command(command.head.value, command.tail.map(_.value))
    }

    val (res, last) = tokens.foldLeft((Seq[Ast](), Seq[Token]())){
      case ((result, acc), Pipe(_)) => (result :+ seqToCommand(acc) :+ Operator(OperatorKind.Pipe), Seq())
      case ((result, acc), And(_)) => (result :+ seqToCommand(acc) :+ Operator(OperatorKind.And), Seq())
      case ((result, acc), Or(_)) => (result :+ seqToCommand(acc) :+ Operator(OperatorKind.Or), Seq())
      case ((result, acc), element) => (result, acc :+ element)
    }

    res :+ seqToCommand(last)
  }

  def substituteHistory(ast: Seq[Ast], history: Seq[String]): Either[Error, Seq[Ast]] = {
    def value(expr: String): Either[Error, Option[String]] = {
      val name = if (expr.startsWith("!")) {
        Some(expr.substring(1))
      } else {
        None
      }

      name match {
        case Some(n) => history.lift(n.toInt) match {
          case None => Left(UndefinedVariable(s"History entry !$n not found"))
          case some => Right(some)
        }
        case None => Right(None)
      }
    }

    ast.foldLeft[Either[Error, Seq[Ast]]](Right(Seq())){
      case (Right(acc), Command(name, args)) =>
        value(name).flatMap{
          case Some(value) =>
            if (args.nonEmpty) {
              Left(HistoryExecNonZeroArguments())
            } else {
              Lexer.lex(value).map(parse)
            }
          case None => Right(Seq(Command(name, args)))
        }.map(acc ++ _)
      case (Right(acc), operator) => Right(acc :+ operator)
      case (left, _) => left
    }
  }

  def substituteVariables(ast: Seq[Ast], variables: Map[String, String]): Either[Error, Seq[Ast]] = {
    def value(expr: String): Either[Error, String] = {
      val name = if (expr.startsWith("@")) {
        Some(expr.substring(1))
      } else {
        None
      }

      name match {
        case Some(n) => variables.get(n) match {
          case Some(value) => Right(value)
          case None => Left(UndefinedVariable(s"Undefined variable @$n"))
        }
        case None => Right(expr)
      }
    }

    substitute(ast, value)
  }

  def substituteEnvVariables(ast: Seq[Ast]): Either[Error, Seq[Ast]] = {
    def value(expr: String): Either[Error, String] = {
      val name = if (expr.startsWith("$")) {
        Some(expr.substring(1))
      } else {
        None
      }

      name match {
        case Some(n) => Properties.envOrNone(n) match {
          case Some(value) => Right(value)
          case None => Left(UndefinedVariable(s"Undefined environment variable $$$n"))
        }
        case None => Right(expr)
      }
    }

    substitute(ast, value)
  }

  def parseReserved(ast: Seq[Ast], cwd: File, history: Seq[String]): Either[Error, (Seq[Ast], File, String)] = {
    ast.headOption match {
      case Some(Command(name, args)) => name match {
        case "cd" =>
          val dir = args.headOption.map(cd(cwd, _)).getOrElse(Right(cwd))
          dir.map(dir => (ast.slice(2, ast.length), dir, s"cd $dir"))
        case "exit" =>
          System.exit(0)
          Right((ast.slice(2, ast.length), cwd, "exit"))
        case "history" =>
          history.zipWithIndex.foreach{ case (line, idx) => println(s"$idx\t$line") }
          Right((ast.slice(2, ast.length), cwd, "history"))
        case _ => Right((ast, cwd, ast.mkString(" ")))
      }
      case _ => Right((ast, cwd, ast.mkString(" ")))
    }
  }

  def parseSet(ast: Seq[Ast], vars: Map[String, String]): Either[Error, (Seq[Ast], Map[String, String])] = {
    ast.headOption match {
      case Some(Command(name, args)) =>name match {
        case "set" =>
          args match {
            case Seq(k, v) => Right((ast.slice(2, ast.length), vars + (k -> v)))
            case _ => Left(InvalidSetArguments())
          }
        case _ => Right((ast, vars))
      }
      case _ => Right((ast, vars))
    }
  }

  def buildProcessTree(ast: Seq[Ast], cwd: File): Either[Error, Option[ProcessBuilder]] = {
    val (builder, op) = ast.foldLeft[(Option[ProcessBuilder], Option[Operator])]((None, None)){
      case ((None, None), Command(name, args)) => (Some(Process.apply(name +: args, cwd)), None)
      case ((Some(proc), None), Operator(kind)) => (Some(proc), Some(Operator(kind)))

      case ((Some(proc), Some(Operator(OperatorKind.Pipe))), Command(name, args)) =>
        (Some(proc #| Process.apply(name +: args, cwd)), None)
      case ((Some(proc), Some(Operator(OperatorKind.And))), Command(name, args)) =>
        (Some(proc #&& Process.apply(name +: args, cwd)), None)
      case ((Some(proc), Some(Operator(OperatorKind.Or))), Command(name, args)) =>
        (Some(proc #|| Process.apply(name +: args, cwd)), None)

      case state => throw new Exception(s"Unexpected console.parser state $state")
    }

    if (op.isDefined) {
      Left(UnexpectedTrailingOperator())
    } else {
      Right(builder)
    }
  }

  private def substitute(ast: Seq[Ast], value: String => Either[Error, String]): Either[Error, Seq[Ast]] = {
    ast.foldLeft[Either[Error, Seq[Ast]]](Right(Seq())){
      case (Right(acc), Command(name, args)) =>
        (name +: args)
          .map(value)
          .foldLeft[Either[Seq[Error], Seq[String]]](Right(Seq())){
            case (Right(seq), Right(str)) => Right(seq :+ str)
            case (Left(errAcc), Left(err)) => Left(errAcc :+ err)
            case (_, Left(err)) => Left(Seq(err))
          }
          .fold(
            errs => Left(MultipleErrors(errs)),
            seq => Right(acc :+ Command(seq.head, seq.tail))
          )
      case (Right(acc), operator) => Right(acc :+ operator)
      case (left, _) => left
    }
  }

  private def cd(cwd: File, to: String): Either[Error, File] = {
    val diff = Paths.get(to)
    if (diff.isAbsolute) {
      diff.normalize.toFile match {
        case file if file.exists() => Right(file)
        case file => Left(PathDoesNotExist(file))
      }
    } else {
      Paths.get(cwd.getAbsolutePath, to).normalize.toFile match {
        case file if file.exists() => Right(file)
        case file => Left(PathDoesNotExist(file))
      }
    }
  }
}
