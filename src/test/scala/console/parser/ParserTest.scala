package console.parser;

import Parser._
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

class ParserTest extends FlatSpec with PrivateMethodTester with Matchers {
  "parse" should "parse a single command" in {
    parse(Seq(Part("ls"))) shouldBe Seq(Command("ls"))
  }

  it should "parse a command with args" in {
    parse(Seq(Part("ls"), Part("-la"))) shouldBe Seq(Command("ls", Seq("-la")))
  }

  it should "parse a command with literal args" in {
    parse(Seq(Part("ls"), Literal("-la"))) shouldBe Seq(Command("ls", Seq("-la")))
  }

  it should "parse pipe" in {
    parse(Seq(Part("ls"), Pipe(), Part("grep"))) shouldBe Seq(
      Command("ls"),
      Operator(OperatorKind.Pipe),
      Command("grep")
    )
  }

  it should "parse and" in {
    parse(Seq(Part("ls"), And(), Part("grep"))) shouldBe Seq(
      Command("ls"),
      Operator(OperatorKind.And),
      Command("grep")
    )
  }

  it should "parse or" in {
    parse(Seq(Part("ls"), Or(), Part("grep"))) shouldBe Seq(
      Command("ls"),
      Operator(OperatorKind.Or),
      Command("grep")
    )
  }

  "substituteHistory" should "substitute !0" in {
    substituteHistory(Seq(Command("!0")), Seq("ls")) shouldBe Right(Seq(Command("ls")))
  }

  it should "substitute !<n>" in {
    substituteHistory(Seq(Command("!1")), Seq("ls", "ls -la | grep")) shouldBe Right(Seq(
      Command("ls", Seq("-la")),
      Operator(OperatorKind.Pipe),
      Command("grep"),
    ))
  }

  it should "fail on non existing history entry" in {
    substituteHistory(Seq(Command("!1")), Seq("ls")) shouldBe Left(UndefinedVariable("History entry !1 not found"))
  }

  "substituteVariables" should "substitute @" in {
    substituteVariables(Seq(Command("@ll")), Map("ll" -> "ls")) shouldBe Right(Seq(Command("ls")))
  }

  it should "fail on missing variable" in {
    substituteVariables(Seq(Command("@ll")), Map()) shouldBe Left(MultipleErrors(Seq(
      UndefinedVariable("Undefined variable @ll")
    )))
  }
}
