package bril.syntax

import cats.Show
import cats.syntax.all._

final case class Destination(destName: String, destType: Type)

object Destination {
  implicit val showInstance: Show[Destination] = new Show[Destination] {
    def show(dest: Destination): String =
      s"${dest.destName}: ${dest.destType.show}"
  }
}
