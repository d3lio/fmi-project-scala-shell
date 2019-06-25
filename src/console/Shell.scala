package console

import java.io.File

import console.parser.{Lexer, Parser}

import scala.io.StdIn
import scala.language.postfixOps
import scala.sys.process._

case class PromptResult(input: String, status: Int, cwd: File, variables: Map[String, String])

class Shell(cwd: File, history: Seq[String] = Seq(), variables: Map[String, String] = Map()) {

  def repl(): Seq[String] = {
    def helper(cwd: File, history: Seq[String], variables: Map[String, String]): Seq[String] = {
      val input = StdIn.readLine(s"${cwd.getAbsolutePath} > ")
      eval(input, cwd, history, variables) match {
        case Some(PromptResult(sub, status, nextCwd, vars)) =>
          if (status != 0) println(s"Process exit status: $status")
          helper(nextCwd, history :+ sub, vars)
        case _ => helper(cwd, history, variables)
      }
    }
    helper(cwd, history, variables)
  }

  private def eval(input: String,
                   cwd: File,
                   history: Seq[String],
                   variables: Map[String, String]): Option[PromptResult] = {
    Lexer
      .lex(input)
      .map(Parser.parse)
      .flatMap(Parser.substituteHistory(_, history))
      .flatMap(Parser.parseSet(_, variables))
      .flatMap{ case (ast, vars) => Parser.substituteVariables(ast, vars).map((_, vars)) }
      .flatMap{ case (ast, vars) => Parser.substituteEnvVariables(ast).map((_, vars)) }
      .flatMap{ case (ast, vars) => Parser.parseReserved(ast, cwd, history).map(t => (t._1, t._2, t._3, vars)) }
      .flatMap{ case (ast, cwd, sub, vars) => Parser.buildProcessTree(ast, cwd).map((_, cwd, sub, vars)) }
    match {
      case Left(err) =>
        println(err.message)
        None
      case Right((Some(process), cwd, sub, vars)) => Some(PromptResult(sub, runProcessTree(process), cwd, vars))
      case Right((_, cwd, sub, vars)) if input.trim.nonEmpty =>
        Some(PromptResult(sub, 0, cwd, vars))
      case _ =>
        None
    }
  }

  private def runProcessTree(process: ProcessBuilder): Int = {
    try {
      process! ProcessLogger(println(_), println(_))
    } catch {
      case ex: Exception =>
        println(s"${ex.getMessage} ${ex.getCause}")
        1
    }
  }
}
