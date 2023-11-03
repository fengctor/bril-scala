package bril.analysis.global

import bril.syntax._

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
}
