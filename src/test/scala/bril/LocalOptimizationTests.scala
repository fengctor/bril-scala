import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import io.circe.parser.decode
import bril._
import bril.syntax._
import bril.analysis._
import bril.analysis.local._
import bril.analysis.local.extensions._
import java.io.ByteArrayOutputStream
import scala.Console

class LocalOptimizationTests extends AnyFunSuite {
  def testLocalOptimization(
    name: String,
    optimization: List[Instruction] => List[Instruction],
    sourceFile: String,
    args: List[Literal]
  ): Unit =
    test(name) {
      val content: String = Source.fromFile(sourceFile).getLines().mkString("\n")
      val Right(program) = decode[Program](content)

      val functionCfgs = program.functions.map { case Function(name, args, retType, instrs) =>
        name -> (args, Cfg.fromInstructions(name, instrs))
      }.toMap
      val optimizedCfgs = functionCfgs.map { case (k, (args, cfg)) =>
        val resultBlocks = cfg.basicBlocks.map { case (blockName, instrs) => (blockName, optimization(instrs)) }
        k -> (args, cfg.copy(basicBlocks = resultBlocks))
      }

      val unoptimizedRunOutputStream = new ByteArrayOutputStream()
      val optimizedRunOutputStream = new ByteArrayOutputStream()

      Console.withOut(unoptimizedRunOutputStream)(Interpreter.run("main", args, functionCfgs))
      Console.withOut(optimizedRunOutputStream)(Interpreter.run("main", args, optimizedCfgs))

      assert(unoptimizedRunOutputStream.toString() == optimizedRunOutputStream.toString())
    }

  testLocalOptimization(
    "dead code elimination",
    DeadCodeElimination.run,
    "src/res/dead_code.json",
    List(IntLit(0))
  )
  testLocalOptimization(
    "copy propagation",
    ValueNumbering.runWithExtension(CopyPropagation.extension),
    "src/res/copy_propagation.json",
    List()
  )
  testLocalOptimization(
    "common subexpression elimination",
    ValueNumbering.runWithExtension(CommonSubexpressionElimination.extension),
    "src/res/common_subexpression_elimination.json",
    List()
  )
  testLocalOptimization(
    "constant propagation",
    ValueNumbering.runWithExtension(ConstantPropagation.extension),
    "src/res/copy_propagation.json",
    List()
  )
  testLocalOptimization(
    "constant folding",
    ValueNumbering.runWithExtension(ConstantPropagation.extension),
    "src/res/constant_folding.json",
    List()
  )
  testLocalOptimization(
    "all LVN-based optimizations",
    ValueNumbering.runWithExtension(
      Extension.pipeAll(
        CommonSubexpressionElimination.extension,
        CopyPropagation.extension,
        ConstantPropagation.extension,
        ConstantFolding.extension
      )
    ),
    "src/res/lvn_composed.json",
    List()
  )
}
