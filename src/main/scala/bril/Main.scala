package bril

import bril.analysis
import bril.syntax._
import cats.syntax.all._

object Main extends App {
  val program1 = Program(
    List(
      Function(
        "main",
        List(Destination("arg", BrilInt)),
        None,
        List(
          Const(Destination("five", BrilInt), IntLit(5)),
          Const(Destination("ten", BrilInt), IntLit(10)),
          Binary(Destination("res", BrilInt), Add, "arg", "five"),
          Binary(Destination("cond", BrilBool), Le, "res", "ten"),
          Br("cond", "then", "else"),
          Label("then"),
          Print(List("res")),
          Label("else")
        )
      )
    )
  )

  val program2 = Program(
    List(
      Function(
        "main",
        List(Destination("arg", BrilInt)),
        None,
        List(
          Label("start"),
          Const(Destination("overwritten", BrilInt), IntLit(0)),
          Const(Destination("five", BrilInt), IntLit(5)),
          Unary(Destination("overwritten", BrilInt), Id, "five"),
          Const(Destination("ten", BrilInt), IntLit(5)),
          Binary(Destination("ten", BrilInt), Add, "ten", "five"),
          Unary(Destination("overwritten", BrilInt), Id, "ten"),
          Binary(Destination("res", BrilInt), Add, "arg", "five"),
          Binary(Destination("overwritten", BrilInt), Add, "res", "res"),
          Binary(Destination("cond", BrilBool), Le, "res", "ten"),
          Binary(Destination("overwritten", BrilInt), Add, "res", "five"),
          Br("cond", "then", "else"),
          Const(Destination("dead", BrilInt), IntLit(0)),
          Label("then"),
          Print(List("res")),
          Ret(Some("five")),
          Label("else"),
          Jmp("start")
        )
      )
    )
  )
  println("Program:")
  println(program2.show)
  println()
  println("CFG:")
  val cfg = analysis.Cfg.fromInstructions("main", program2.functions(0).instrs)
  println(cfg.show)
  println()
  println("After local dead code elimination:")
  val blocksEliminated = analysis.local.DeadCodeElimination.run(cfg.basicBlocks)
  println(cfg.copy(basicBlocks = blocksEliminated).show)
}
