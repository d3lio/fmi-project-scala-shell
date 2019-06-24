package console.parser

object Parser {
  sealed trait Error { def message: String }
  case class UnclosedQuote(message: String = "Unclosed quote in input") extends Error
  case class EmptyCommand(message: String = "Empty command due to badly placed operator") extends Error

  /**
   * Parse the input string and return a sequence of sequences where the head of each sequence
   * is the name of the command and the tail represents the arguments.
   */
  def parse(input: String): Either[Error, Seq[Ast]] = {
    Right(splitStream(input, """(?<!\\)\"""", -1))
      .flatMap(parseLiterals)
      .map(removeEmptyParts)
      .flatMap(parseOperator(_, """\|\|""", Or()))
      .flatMap(parseOperator(_, "&&", And()))
      .flatMap(parseOperator(_, """\|""", Pipe()))
      .map(tokens => tokens.flatMap{
        case Unparsed(part) => part.split(" ").toSeq.map(Part)
        case other => Seq(other)
      })
      .map(seqSplitByAndPreserve(_, Seq(Or(), And(), Pipe())))
      .map(parseAst)
  }

  private def splitStream(input: String, regex: String): Seq[Unparsed] =
    input.split(regex).toSeq.map(Unparsed)
  private def splitStream(input: String, regex: String, limit: Int): Seq[Unparsed] =
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

  private def parseOperator(tokens: Seq[Token], regex: String, token: Token): Either[Error, Seq[Token]] = {
    val parsed = tokens.flatMap {
      case Unparsed(part) => splitStream(part, regex)
        .map(token => Unparsed(token.value.trim))
        .flatMap(Seq(token, _))
        .tail
      case other => Seq(other)
    }

    parsed.indexOf(Unparsed("")) match {
      case -1 => Right(parsed)
      case _ => Left(EmptyCommand())
    }
  }

  private def seqSplitByAndPreserve[T](seq: Seq[T], items: Seq[T]): Seq[Seq[T]] = {
    val (res, last) = seq.foldLeft((Seq[Seq[T]](), Seq[T]())){
      case ((result, acc), element) if items.contains(element) => (result :+ acc :+ Seq(element), Seq())
      case ((result, acc), element) => (result, acc :+ element)
    }
    res :+ last
  }

  private def parseAst(commands: Seq[Seq[Token]]): Seq[Ast] = {
    commands.map{
      case Seq(Pipe(_)) => Operator(OperatorKind.Pipe)
      case Seq(And(_)) => Operator(OperatorKind.And)
      case Seq(Or(_)) => Operator(OperatorKind.Or)
      case tokens => Command(tokens.head.value, tokens.tail.map(_.value))
    }
  }
}
