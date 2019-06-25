package console.parser

import Lexer._
import org.scalatest.{FlatSpec, Matchers}

class LexerTest extends FlatSpec with Matchers {
  "lex" should "parse a single command" in {
    lex("ls") shouldBe Right(Seq(Part("ls")))
  }

  it should "parse arguments" in {
    lex("""ls -la""") shouldBe Right(Seq(Part("ls"), Part("-la")))
  }

  it should "parse literal strings" in {
    lex("""ls "Program Files"""") shouldBe Right(Seq(Part("ls"), Literal("Program Files")))
  }

  it should "parse literal strings with escaped quote" in {
    lex("""ls "a\"b"""") shouldBe Right(Seq(Part("ls"), Literal("""a\"b""")))
  }

  it should "parse pipe" in {
    lex("ls | grep") shouldBe Right(Seq(Part("ls"), Pipe(), Part("grep")))
  }

  it should "parse and" in {
    lex("ls && grep") shouldBe Right(Seq(Part("ls"), And(), Part("grep")))
  }

  it should "parse or" in {
    lex("ls || grep") shouldBe Right(Seq(Part("ls"), Or(), Part("grep")))
  }

  it should "parse compound expressions" in {
    lex("""ls -la | grep -v "Scala" && cd ..""") shouldBe Right(Seq(
      Part("ls"), Part("-la"),
      Pipe(),
      Part("grep"), Part("-v"), Literal("Scala"),
      And(),
      Part("cd"), Part("..")
    ))
  }

  it should "fail when quotes are not closed" in {
    lex("""ls """") shouldBe Left(Parser.UnclosedQuote())
  }
}
