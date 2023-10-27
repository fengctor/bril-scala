package bril.syntax

import cats.Show
import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._

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

  implicit val encodeInstance: Encoder[Type] = Encoder.instance(_.show.asJson)

  implicit val decoderInstance: Decoder[Type] = Decoder.decodeString.emap {
    case "int"  => Right(BrilInt)
    case "bool" => Right(BrilBool)
    case t      => Left(s"Unknown type $t")
  }
}
