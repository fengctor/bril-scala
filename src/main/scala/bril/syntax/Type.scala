package bril.syntax

import cats.Show
import cats.syntax.all._

sealed trait Type
final case object BrilInt extends Type
final case object BrilBool extends Type

object Type {
  implicit val showInstance: Show[Type] = new Show[Type] {
    override def show(t: Type): String = t match {
      case BrilInt  => "int"
      case BrilBool => "bool"
    }
  }
}
