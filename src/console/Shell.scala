package console

import parser._

import scala.io.StdIn
import scala.sys.process._

case class PromptResult(line: String, stdout: String, stderr: String, status: Int)

// TODO alias history cd exit
class Shell(cwd: String, aliases: Map[String, String] = Map()) {

  def repl(): Seq[String] = {
    def helper(history: Seq[String]): Seq[String] = {
      val result = prompt()
      helper(history :+ result.line)
    }
    helper(Seq())
  }

  private def prompt(): PromptResult = {
    val input = StdIn.readLine(s"$cwd > ")
    this.parseInput(input)

    PromptResult(input, "", "", 0)
  }

  private def parseInput(input: String): Seq[Process] = {
    Parser.parse(input) match {
      case Left(err: Parser.Error) =>
        println(s"ParseError: $err.message")
        System.exit(1)

      case Right(ast: Seq[Ast]) =>
        ast.map {
          case Command(name, args) => Command(aliases.getOrElse(name, name), args)
          case x => x
        }
          .foreach(println(_))
//        .map(command => ProcessBuilder(command, cwd))
    }

    Seq()
  }
}
