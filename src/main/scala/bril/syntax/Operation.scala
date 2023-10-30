package bril.syntax

import cats.Show
import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._
import scala.util.Try

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

  def fromString(s: String): BinaryOperation = s match {
    case "add" => Add
    case "mul" => Mul
    case "sub" => Sub
    case "div" => Div
    case "eq"  => Eq
    case "lt"  => Lt
    case "gt"  => Gt
    case "le"  => Le
    case "ge"  => Ge
    case "and" => And
    case "or"  => Or
    case _     => throw new Exception(s"Invalid BinaryOperation: $s")
  }

  implicit val encodeInstance: Encoder[BinaryOperation] = Encoder.instance[BinaryOperation](_.show.asJson)

  implicit val decoderInstance: Decoder[BinaryOperation] = Decoder.decodeString.emapTry(s => Try(fromString(s)))
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

  def fromString(s: String): UnaryOperation = s match {
    case "not" => Not
    case "id"  => Id
    case _     => throw new Exception(s"Invalid UnaryOperation: $s")
  }

  implicit val encodeInstance: Encoder[UnaryOperation] = Encoder.instance[UnaryOperation](_.show.asJson)

  implicit val decoderInstance: Decoder[UnaryOperation] = Decoder.decodeString.emapTry(s => Try(fromString(s)))

}
