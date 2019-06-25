package console.parser

object OperatorKind extends Enumeration {
  type OperatorKind = Value
  val Pipe, Or, And = Value
}
import OperatorKind._

sealed trait Ast
case class Command(name: String, args: Seq[String] = Seq()) extends Ast {
  override def toString: String = (name +: args).mkString(" ")
}
case class Operator(kind: OperatorKind) extends Ast {
  override def toString: String = kind match {
    case OperatorKind.Pipe => "|"
    case OperatorKind.And => "&&"
    case OperatorKind.Or => "||"
  }
}
