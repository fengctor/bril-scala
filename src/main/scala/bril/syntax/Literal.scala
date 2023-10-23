package bril.syntax

import cats.Show
import cats.syntax.all._

sealed trait Literal
final case class IntLit(value: Int) extends Literal
final case class BoolLit(value: Boolean) extends Literal

object Literal {
  implicit val showInstance: Show[Literal] = new Show[Literal] {
    def show(lit: Literal): String = lit match {
      case IntLit(value)  => s"$value"
      case BoolLit(value) => s"$value"
    }
  }
}
