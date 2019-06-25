package console.parser

import java.io.File

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
}
