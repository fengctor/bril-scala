package bril

import bril.analysis
import bril.analysis.Cfg
import bril.syntax._
import cats.Show
import cats.syntax.all._
import io.circe.parser.decode
import io.circe.syntax._
import java.io.File
import java.io.PrintWriter
import scala.io.Source

object Main extends App {
  if (args.length < 1) sys.exit(1)

  val filename :: progArgStrs = args.toList
  val progArgValues = progArgStrs.map(s => IntLit(s.toInt))
  val content: String = Source.fromFile(filename).getLines().mkString("\n")
  val Right(program) = decode[Program](content)
  val progArgNames = program.functions(0).args

  println("Program:")
  println(program.show)
  println()
  println("JSON:")
  println(program.asJson)
  println()
  println("CFG:")
  val cfg = Cfg.fromInstructions("main", program.functions(0).instrs)
  println(cfg.show)
  println()
  println("Result from running:")
  Interpreter.run("main", progArgValues, Map("main" -> (progArgNames, cfg)))
  println()

  def testLocalOptimization(name: String, optimization: List[Instruction] => List[Instruction]): Unit = {
    println(s"After $name:")
    val resultBlocks = cfg.basicBlocks.map { case (blockName, instrs) => (blockName, optimization(instrs)) }
    // Do dead code elimination
    val resultCfg = cfg.copy(basicBlocks = resultBlocks)
    val liveVariableResults = analysis.global.Dataflow.run(
      analysis.global.Dataflow.Backwards,
      analysis.global.LiveVariables.init,
      analysis.global.LiveVariables.merge,
      analysis.global.LiveVariables.transfer
    )(resultCfg)
    val dceBlocks = resultBlocks.map { case (blockName, instrs) =>
      (
        blockName,
        analysis.global.DeadCodeElimination.localWithLiveVariables(liveVariableResults.out(blockName), instrs)
      )
    }
    val finalCfg = cfg.copy(basicBlocks = dceBlocks)

    println(finalCfg.show)
    println()
    println("Result from running:")
    Interpreter.run("main", progArgValues, Map("main" -> (progArgNames, finalCfg)))
    println()
  }

  def testGlobalAnalysis[T: Show](name: String, analyze: Cfg => analysis.global.Dataflow.Results[T]): Unit = {
    println(s"Results for $name:")
    println(analyze(cfg).show)
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

  testGlobalAnalysis(
    "reaching definitions",
    analysis.global.ReachingDefinitions.run(program.functions(0))
  )

  testGlobalAnalysis(
    "live variables",
    analysis.global.LiveVariables.run
  )

  testGlobalAnalysis(
    "constant propagation",
    analysis.global.ConstantPropagation.run(program.functions(0))
  )

}
