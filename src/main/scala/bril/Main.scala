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
          Binary(Destination("overwritten", BrilInt), Add, "arg", "five"),
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

  val lvnProgram = Program(
    List(
      Function(
        "main",
        List(),
        None,
        List(
          Const(Destination("a", BrilInt), IntLit(4)),
          Const(Destination("b", BrilInt), IntLit(2)),
          Binary(Destination("sum1", BrilInt), Add, "a", "b"),
          Binary(Destination("sum2", BrilInt), Add, "a", "b"),
          Binary(Destination("prod", BrilInt), Mul, "sum1", "sum2"),
          Print(List("a")),
          Binary(Destination("a", BrilInt), Mul, "sum1", "sum2"),
          Print(List("prod"))
        )
      )
    )
  )

  val copyPropProgram = Program(
    List(
      Function(
        "main",
        List(),
        None,
        List(
          Const(Destination("x", BrilInt), IntLit(4)),
          Unary(Destination("copy1", BrilInt), Id, "x"),
          Unary(Destination("copy2", BrilInt), Id, "copy1"),
          Unary(Destination("copy3", BrilInt), Id, "copy2"),
          Print(List("copy3"))
        )
      )
    )
  )

  val cseProgram = Program(
    List(
      Function(
        "main",
        List(),
        None,
        List(
          Const(Destination("a", BrilInt), IntLit(4)),
          Const(Destination("b", BrilInt), IntLit(2)),
          Binary(Destination("sum1", BrilInt), Add, "a", "b"),
          Binary(Destination("sum2", BrilInt), Add, "b", "a"),
          Binary(Destination("prod", BrilInt), Mul, "sum1", "sum2"),
          Print(List("a")),
          Binary(Destination("a", BrilInt), Mul, "sum1", "sum2"),
          Print(List("prod"))
        )
      )
    )
  )


  val program = cseProgram
  println("Program:")
  println(program.show)
  println()
  println("CFG:")
  val cfg = analysis.Cfg.fromInstructions("main", program.functions(0).instrs)
  println(cfg.show)
  println()

  def testLocalOptimization(name: String, optimization: List[Instruction] => List[Instruction]): Unit = {
    println(s"After $name:")
    var resultBlocks = cfg.basicBlocks.map { case (blockName, instrs) => (blockName, optimization(instrs)) }
    val resultCfg = cfg.copy(basicBlocks = resultBlocks)
    println(resultCfg.show)
    println()
  }

  testLocalOptimization(
    "dead code elimination",
    analysis.local.DeadCodeElimination.run
  )
  testLocalOptimization(
    "local value numbering",
    analysis.local.ValueNumbering.runWithExtension(
      analysis.local.ValueNumbering.extension
    )
  )
  testLocalOptimization(
    "copy propagation",
    analysis.local.ValueNumbering.runWithExtension(
      analysis.local.extensions.CopyPropagation.extension
    )
  )
  testLocalOptimization(
    "common subexpression elimination",
    analysis.local.ValueNumbering.runWithExtension(
      analysis.local.extensions.CommonSubexpressionElimination.extension
    )
  )
  testLocalOptimization(
    "constant propagation",
    analysis.local.ValueNumbering.runWithExtension(
      analysis.local.extensions.ConstantPropagation.extension
    )
  )
}
