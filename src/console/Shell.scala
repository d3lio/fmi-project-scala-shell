package console

import java.io.File

import console.parser.{Lexer, Parser}

import scala.io.StdIn
import scala.language.postfixOps
import scala.sys.process._

case class PromptResult(input: String, status: Int, cwd: File)

// TODO setting vars, source
class Shell(cwd: File, history: Seq[String] = Seq(), variables: Map[String, String] = Map()) {

  def repl(): Seq[String] = {
    def helper(cwd: File, history: Seq[String]): Seq[String] = {
      prompt(cwd, history) match {
        case Some(PromptResult(input, status, nextCwd)) =>
          if (status != 0) println(s"Process exit status: $status")
          helper(nextCwd, history :+ input)
        case _ => helper(cwd, history)
      }
    }
    helper(cwd, history)
  }

  private def prompt(cwd: File, history: Seq[String]): Option[PromptResult] = {
    val input = StdIn.readLine(s"${cwd.getAbsolutePath} > ")

    Lexer
      .lex(input)
      .map(Parser.parse)
      .flatMap(Parser.substituteHistory(_, history))
      .flatMap(Parser.substituteVariables(_, variables))
      .flatMap(Parser.substituteEnvVariables)
      .flatMap(Parser.parseReserved(_, cwd, history))
      .flatMap{ case (ast, cwd, substituted) => Parser.buildProcessTree(ast, cwd).map((_, cwd, substituted)) }
    match {
      case Left(err) =>
        println(err.message)
        None
      case Right((Some(process), cwd, substituted)) => Some(PromptResult(substituted, runProcessTree(process), cwd))
      case Right((_, cwd, substituted)) if input.trim.nonEmpty =>
        Some(PromptResult(substituted, 0, cwd))
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
