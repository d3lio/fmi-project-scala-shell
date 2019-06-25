package console.parser

import Parser.{Error, UnclosedQuote}

object Lexer {
  def lex(input: String): Either[Error, Seq[Token]] = {
    Right(splitStream(input, """(?<!\\)\"""", -1))
      .flatMap(parseLiterals)
      .map(removeEmptyParts)
      .map(parseOperator(_, """\|\|""", Or()))
      .map(parseOperator(_, """\&\&""", And()))
      .map(parseOperator(_, """\|""", Pipe()))
      .map(tokens => tokens.flatMap{
        case Unparsed(part) => part.split(" ").toSeq.map(Part)
        case other => Seq(other)
      })
  }

  private def splitStream(input: String, regex: String): Seq[Token] =
    input.split(regex).toSeq.map(Unparsed)
  private def splitStream(input: String, regex: String, limit: Int): Seq[Token] =
    input.split(regex, limit).toSeq.map(Unparsed)

  private def removeEmptyParts(tokens: Seq[Token]): Seq[Token] = {
    tokens.filter{
      case Unparsed(value) => value.trim.nonEmpty
      case _ => true
    }
  }

  private def parseLiterals(tokens: Seq[Token]): Either[Error, Seq[Token]] = {
    if (tokens.length % 2 == 1) {
      Right(tokens.zipWithIndex.map{
        case (t, i) => (t, i % 2)
      }.map{
        case (Unparsed(token), 1) => Literal(token)
        case (token, 0) => token
      })
    } else {
      Left(UnclosedQuote())
    }
  }

  private def parseOperator(tokens: Seq[Token], regex: String, token: Token): Seq[Token] = {
    val parsed = tokens.flatMap {
      case Unparsed(part) => splitStream(part, regex)
        .map(token => Unparsed(token.value.trim))
        .flatMap(Seq(token, _))
        .tail
      case other => Seq(other)
    }
    removeEmptyParts(parsed)
  }
}
