package bril

import bril.analysis
import bril.syntax._
import cats.syntax.all._
import io.circe.parser.decode
import io.circe.syntax._
import java.io.File
import java.io.PrintWriter
import scala.io.Source

object Main extends App {
  if (args.length != 1) sys.exit(1)

  val filename = args(0)
  val content: String = Source.fromFile(filename).getLines().mkString("\n")
  val Right(program) = decode[Program](content)

  println("Program:")
  println(program.show)
  println()
  println("JSON:")
  println(program.asJson)
  println()
  println("CFG:")
  val cfg = analysis.Cfg.fromInstructions("main", program.functions(0).instrs)
  println(cfg.show)
  println()
  println("Result from running:")
  Interpreter.run("main", Nil, Map("main" -> (Nil, cfg)))
  println()

  def testLocalOptimization(name: String, optimization: List[Instruction] => List[Instruction]): Unit = {
    println(s"After $name:")
    var resultBlocks = cfg.basicBlocks.map { case (blockName, instrs) => (blockName, optimization(instrs)) }
    val resultCfg = cfg.copy(basicBlocks = resultBlocks)
    println(resultCfg.show)
    println()
    println("Result from running:")
    Interpreter.run("main", Nil, Map("main" -> (Nil, resultCfg)))
    println()
  }

  testLocalOptimization(
    "dead code elimination",
    analysis.local.DeadCodeElimination.run
  )
  testLocalOptimization(
    "local value numbering",
    analysis.local.ValueNumbering.runWithExtension(
      analysis.local.extensions.Extension.idExtension
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
  testLocalOptimization(
    "constant folding",
    analysis.local.ValueNumbering.runWithExtension(
      analysis.local.extensions.ConstantFolding.extension
    )
  )
  testLocalOptimization(
    "all lvn extensions",
    analysis.local.ValueNumbering.runWithExtension(
      analysis.local.extensions.Extension.pipeAll(
        analysis.local.extensions.CommonSubexpressionElimination.extension,
        analysis.local.extensions.CopyPropagation.extension,
        analysis.local.extensions.ConstantPropagation.extension,
        analysis.local.extensions.ConstantFolding.extension
      )
    )
  )
}
