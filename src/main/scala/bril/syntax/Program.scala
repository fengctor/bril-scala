package bril.syntax

import cats.Show
import cats.syntax.all._
import io.circe.{Decoder, Encoder}

final case class Program(functions: List[Function])

object Program {
  implicit val showInstance: Show[Program] = new Show[Program] {
    override def show(program: Program): String =
      program.functions.map(_.show).mkString("\n\n")
  }

  implicit val encoderInstance: Encoder[Program] =
    Encoder.forProduct1("functions")(p => p.functions)

  implicit val decoderInstance: Decoder[Program] =
    Decoder.forProduct1("functions")(functions => Program(functions))

}
