package console

import parser._

import scala.io.StdIn
import scala.language.postfixOps
import scala.sys.process._
import java.io.File

case class PromptResult(input: String, status: Int)

// TODO alias or $var, source, history, cd, exit, !command_no, printvars
class Shell(cwd: File, history: Seq[String] = Seq(), variables: Map[String, String] = Map()) {

  def repl(): Seq[String] = {
    def helper(history: Seq[String]): Seq[String] = {
      prompt() match {
        case Some(PromptResult(input, status)) =>
          if (status != 0) println(s"Process exit status: $status")
          helper(history :+ input)
        case _ => helper(history)
      }
    }
    helper(history)
  }

  private def prompt(): Option[PromptResult] = {
    val input = StdIn.readLine(s"${cwd.getAbsolutePath} > ")

    Lexer
      .lex(input)
      .map(Parser.parse)
      .flatMap(Parser.substituteVariables(_, variables))
      .flatMap(Parser.buildProcessTree(_, cwd))

    match {
      case Left(err) =>
        println(err.message)
        None
      case Right(Some(process)) => Some(PromptResult(input, runProcessTree(process)))
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
