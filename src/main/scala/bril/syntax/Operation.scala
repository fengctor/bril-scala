package bril.syntax

import cats.Show
import cats.syntax.all._

sealed trait BinaryOperation
final case object Add extends BinaryOperation
final case object Mul extends BinaryOperation
final case object Sub extends BinaryOperation
final case object Div extends BinaryOperation
final case object Eq extends BinaryOperation
final case object Lt extends BinaryOperation
final case object Gt extends BinaryOperation
final case object Le extends BinaryOperation
final case object Ge extends BinaryOperation
final case object And extends BinaryOperation
final case object Or extends BinaryOperation

object BinaryOperation {
  implicit val showInstance: Show[BinaryOperation] = new Show[BinaryOperation] {
    override def show(op: BinaryOperation): String = op match {
      case Add => "add"
      case Mul => "mul"
      case Sub => "sub"
      case Div => "div"
      case Eq  => "eq"
      case Lt  => "lt"
      case Gt  => "gt"
      case Le  => "le"
      case Ge  => "ge"
      case And => "and"
      case Or  => "or"
    }
  }
}

sealed trait UnaryOperation
final case object Not extends UnaryOperation
final case object Id extends UnaryOperation

object UnaryOperation {
  implicit val showInstance: Show[UnaryOperation] = new Show[UnaryOperation] {
    override def show(op: UnaryOperation): String = op match {
      case Not => "not"
      case Id  => "id"
    }
  }
}
