package console.parser

object OperatorKind extends Enumeration {
  type OperatorKind = Value
  val Pipe, Or, And = Value
}
import console.parser.OperatorKind.OperatorKind

sealed trait Ast
case class Command(name: String, args: Seq[String]) extends Ast {
  override def toString: String = (name +: args).mkString(" ")
}
case class Operator(kind: OperatorKind) extends Ast {
  override def toString: String = kind match {
    case OperatorKind.Pipe => "|"
    case OperatorKind.And => "&&"
    case OperatorKind.Or => "||"
  }
}
