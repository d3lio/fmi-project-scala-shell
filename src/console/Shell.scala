package console

import parser._

import scala.io.StdIn
import java.io.File

case class PromptResult(line: String, stdout: String, stderr: String, status: Int)

// TODO alias, source, history, cd, exit
class Shell(cwd: File, history: Seq[String] = Seq(), aliases: Map[String, String] = Map()) {

  def repl(): Seq[String] = {
    def helper(history: Seq[String]): Seq[String] = {
      val result = prompt()
      helper(history :+ result.line)
    }
    helper(history)
  }

  private def prompt(): PromptResult = {
    val input = StdIn.readLine(s"$cwd > ")
    Parser.parse(input, cwd, aliases) match {
      case Left(err) => println(err)
      case Right(Some(builder)) => println(builder)
      case _ =>
    }

    PromptResult(input, "", "", 0)
  }
}
