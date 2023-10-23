package bril.syntax

import cats.Show
import cats.syntax.all._

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
}
