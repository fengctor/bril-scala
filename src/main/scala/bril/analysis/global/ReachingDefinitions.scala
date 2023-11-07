package bril.analysis.global

import bril.syntax._
import bril.analysis.Cfg
import bril.analysis.global.Dataflow

object ReachingDefinitions {
  // Union of two Maps from variable names to their sets of definitions, taking the union of values on conflicting names
  def unionCombiningDefs(
    m1: Map[String, Set[Instruction]],
    m2: Map[String, Set[Instruction]]
  ): Map[String, Set[Instruction]] = m2.foldLeft(m1) { (acc, cur) =>
    val (varName, defs) = cur
    acc.updated(varName, acc.getOrElse(varName, Set.empty) ++ defs)
  }
  // TODO: how to handle arguments? Dummy instructions?
  def init(f: Function): Map[String, Set[Instruction]] = Map.empty

  def merge(predOuts: List[Map[String, Set[Instruction]]]): Map[String, Set[Instruction]] =
    predOuts.foldLeft(Map.empty[String, Set[Instruction]])(unionCombiningDefs)

  def transfer(block: List[Instruction], curIn: Map[String, Set[Instruction]]): Map[String, Set[Instruction]] = {
    // Locally, only the last definition for a given variable matters
    def findDefs(block: List[Instruction], accDefs: Map[String, Instruction]): Map[String, Instruction] = block match {
      case Nil => accDefs
      case instr :: instrs =>
        Instruction.dest(instr) match {
          case None                           => findDefs(instrs, accDefs)
          case Some(Destination(destName, _)) => findDefs(instrs, accDefs + (destName -> instr))
        }
    }
    val defs = findDefs(block, Map.empty)
    // Replace definitions for a variable in curIn with the ones in defs;
    // implicitly handles removing the KILLS set
    curIn ++ defs.map { case (k, v) => k -> Set(v) }
  }

  def run(f: Function)(cfg: Cfg): Dataflow.Results[Map[String, Set[Instruction]]] =
    Dataflow.run(Dataflow.Forwards, init(f), merge, transfer)(cfg)
}
