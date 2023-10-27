package bril.syntax

import cats.Show
import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._
import scala.util.Try

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

  implicit val encodeInstance: Encoder[Literal] = Encoder.instance {
    case IntLit(n)  => n.asJson
    case BoolLit(b) => b.asJson
  }

  val intDecoder: Decoder[Literal] = Decoder.decodeInt.map(n => IntLit(n))
  val boolDecoder: Decoder[Literal] = Decoder.decodeBoolean.map(b => BoolLit(b))
  implicit val decoderInstance: Decoder[Literal] = intDecoder or boolDecoder
}
