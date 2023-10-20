sealed trait Instruction
final case class Label(label: String) extends Instruction
final case class Const(dest: String, destType: Type, value: Literal) extends Instruction
final case class Binary(op: BinaryOperation, argLeft: String, argRight: String) extends Instruction
final case class Unary(op: UnaryOperation, arg: String) extends Instruction
final case class Jmp(label: String) extends Instruction
final case class Br(arg: String, label1: String, label2: String) extends Instruction
final case class Call(dest: Option[String], name: String, args: List[String]) extends Instruction
final case class Ret(arg: Option[String]) extends Instruction
final case class Print(args: List[String]) extends Instruction
final case object Nop extends Instruction
