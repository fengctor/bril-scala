final case class Function(
  name: String,
  args: List[(String, Type)],
  retType: Option[Type],
  instrs: List[Instruction]
)
