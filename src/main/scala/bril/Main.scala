package bril

import bril.analysis._
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
          Const(Destination("five", BrilInt), IntLit(5)),
          Const(Destination("ten", BrilInt), IntLit(10)),
          Binary(Destination("res", BrilInt), Add, "arg", "five"),
          Binary(Destination("cond", BrilBool), Le, "res", "ten"),
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
  println(Cfg.fromInstructions("main", program2.functions(0).instrs).show)
}
