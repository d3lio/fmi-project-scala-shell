package console.parser

object OperatorKind extends Enumeration {
  type OperatorKind = Value
  val Pipe, Or, And = Value
}
import OperatorKind._

sealed trait Ast
case class Command(name: String, args: Seq[String]) extends Ast
case class Operator(kind: OperatorKind) extends Ast
