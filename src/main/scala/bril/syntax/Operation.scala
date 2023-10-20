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

sealed trait UnaryOperation
final case object Not extends UnaryOperation
final case object Id extends UnaryOperation
