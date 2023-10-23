package bril.syntax

import cats.Show
import cats.syntax.all._

final case class Program(functions: List[Function])

object Program {
  implicit val showInstance: Show[Program] = new Show[Program] {
    override def show(program: Program): String =
      program.functions.map(_.show).mkString("\n\n")
  }
}
