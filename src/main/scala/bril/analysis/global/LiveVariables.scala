package bril.analysis.global

import bril.syntax._
import bril.analysis.Cfg

object LiveVariables {
  val init: Set[String] = Set.empty

  def merge(succIns: List[Set[String]]): Set[String] = succIns.foldLeft(Set.empty[String])(_ ++ _)

  def transfer(block: List[Instruction], curOut: Set[String]): Set[String] = {
    // Accumulate the used and killed variables
    val (used, killed) = block.foldLeft((Set.empty[String], Set.empty[String])) { (acc, curInstr) =>
      val (accUsed, accKilled) = acc
      val nextUsed = accUsed ++ Instruction.args(curInstr)
      val nextKilled = Instruction.dest(curInstr) match {
        case None                           => accKilled
        case Some(Destination(destName, _)) => accKilled + destName
      }
      (nextUsed, nextKilled)
    }
    used ++ (curOut -- killed)
  }

  def run(cfg: Cfg): Dataflow.Results[Set[String]] =
    Dataflow.run(Dataflow.Backwards, init, merge, transfer)(cfg)
}
