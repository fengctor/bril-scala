package bril.syntax

import cats.Show
import cats.syntax.all._
import io.circe.{Decoder, Encoder}

final case class Destination(destName: String, destType: Type)

object Destination {
  implicit val showInstance: Show[Destination] = new Show[Destination] {
    def show(dest: Destination): String =
      s"${dest.destName}: ${dest.destType.show}"
  }

  implicit val encoderInstance: Encoder[Destination] =
    Encoder.forProduct2("name", "type") { d =>
      (d.destName, d.destType)
    }

  implicit val decoderInstance: Decoder[Destination] =
    Decoder.forProduct2("name", "type") { (destName, destType) =>
      Destination(destName, destType)
    }
}
