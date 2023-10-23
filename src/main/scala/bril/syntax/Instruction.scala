package bril.syntax

import cats.Show
import cats.syntax.all._

sealed trait Instruction
final case class Label(label: String) extends Instruction
final case class Const(dest: Destination, value: Literal) extends Instruction
final case class Binary(dest: Destination, op: BinaryOperation, argLeft: String, argRight: String) extends Instruction
final case class Unary(dest: Destination, op: UnaryOperation, arg: String) extends Instruction
final case class Jmp(label: String) extends Instruction
final case class Br(arg: String, label1: String, label2: String) extends Instruction
final case class Call(optDest: Option[Destination], name: String, args: List[String]) extends Instruction
final case class Ret(arg: Option[String]) extends Instruction
final case class Print(args: List[String]) extends Instruction
final case object Nop extends Instruction

object Instruction {
  implicit val showInstance: Show[Instruction] = new Show[Instruction] {
    override def show(instr: Instruction): String = instr match {
      case Label(label)                        => s".$label"
      case Const(dest, value)                  => s"${dest.show} = const ${value.show}"
      case Binary(dest, op, argLeft, argRight) => s"${dest.show} = ${op.show} $argLeft $argRight"
      case Unary(dest, op, arg)                => s"${dest.show} = ${op.show} $arg"
      case Jmp(label)                          => s"jmp .$label"
      case Br(arg, label1, label2)             => s"br $arg .$label1 .$label2"
      case Call(optDest, name, args) =>
        val rhsStr = s"$name ${args.mkString(" ")}"
        optDest match {
          case None       => rhsStr
          case Some(dest) => s"${dest.show} = $rhsStr"
        }
      case Ret(arg)    => s"print $arg"
      case Print(args) => s"ret ${args.mkString(" ")}"
      case Nop         => "nop"
    }
  }
}
