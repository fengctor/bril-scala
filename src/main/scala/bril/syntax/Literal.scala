sealed trait Literal
final case class Int(value: Int) extends Literal
final case class Bool(value: Boolean) extends Literal
