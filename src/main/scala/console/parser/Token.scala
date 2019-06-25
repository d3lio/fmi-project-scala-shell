package console.parser

sealed trait Token {
  def value: String
}

case class Unparsed(value: String) extends Token
case class Literal(value: String) extends Token
case class Part(value: String) extends Token
case class Pipe(value: String = "|") extends Token
case class Or(value: String = "||") extends Token
case class And(value: String = "&&") extends Token
