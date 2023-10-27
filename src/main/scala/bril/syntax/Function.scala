package bril.syntax

import cats.Show
import cats.syntax.all._
import io.circe.{Decoder, Encoder}

final case class Function(
  name: String,
  args: List[Destination],
  retType: Option[Type],
  instrs: List[Instruction]
)

object Function {
  implicit val showInstance: Show[Function] = new Show[Function] {
    override def show(function: Function): String = {
      val argsStr =
        if (function.args.isEmpty) ""
        else {
          val inner = function.args.map(_.show).mkString(", ")
          s"($inner)"
        }
      val retTypeStr = function.retType.map(t => s": ${t.show}").getOrElse("")
      val instrsStr = function.instrs
        .map { instr =>
          val baseStr = instr.show
          // indent non-label instructions
          instr match {
            case Label(_) => s"$baseStr:"
            case _        => s"  $baseStr;"
          }
        }
        .mkString("\n")

      s"@${function.name}$argsStr$retTypeStr {\n$instrsStr\n}"
    }
  }

  implicit val encoderInstance: Encoder[Function] =
    Encoder
      .forProduct4("name", "args", "type", "instrs") { (f: Function) =>
        (f.name, if (f.args.isEmpty) None else Some(f.args), f.retType, f.instrs)
      }
      .mapJson(_.dropNullValues)

  implicit val decoderInstance: Decoder[Function] =
    Decoder.forProduct4("name", "args", "type", "instrs") {
      (name: String, optArgs: Option[List[Destination]], retType: Option[Type], instrs: List[Instruction]) =>
        Function(name, optArgs.getOrElse(Nil), retType, instrs)
    }
}
