package console

import java.io.File

import console.parser.Lexer

import scala.io.StdIn
import scala.language.postfixOps
import scala.sys.process._

case class PromptResult(input: String, status: Int, cwd: File)

// TODO settings vars, source, history, cd, exit, !command_no, printvars
class Shell(cwd: File, history: Seq[String] = Seq(), variables: Map[String, String] = Map()) {

  def repl(): Seq[String] = {
    def helper(cwd: File, history: Seq[String]): Seq[String] = {
      prompt(cwd) match {
        case Some(PromptResult(input, status, nextCwd)) =>
          if (status != 0) println(s"Process exit status: $status")
          helper(nextCwd, history :+ input)
        case _ => helper(cwd, history)
      }
    }
    helper(cwd, history)
  }

  private def prompt(cwd: File): Option[PromptResult] = {
    val input = StdIn.readLine(s"${cwd.getAbsolutePath} > ")

    Lexer
      .lex(input)
      .map(parser.Parser.parse)
      .flatMap(parser.Parser.substituteVariables(_, variables))
      .flatMap(parser.Parser.substituteEnvVariables)
      .flatMap(parser.Parser.buildProcessTree(_, cwd))
    match {
      case Left(err) =>
        println(err.message)
        None
      case Right(Some(process)) => Some(PromptResult(input, runProcessTree(process), cwd))
      case _ => None
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
