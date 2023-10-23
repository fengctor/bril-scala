package bril.analysis.local

import bril.syntax._
import cats.Functor
import cats.syntax.all._

object DeadCodeElimination {
  private def elimInBlock(block: List[Instruction]): List[Instruction] = {
    // i: index of current instruction
    // lastDef: (arg -> index of last seen unused definition)
    // returns a set of indices of instructions in the block to remove
    def findInstrsToRemove(i: Int, block: List[Instruction], lastDef: Map[String, Int], toRemove: Set[Int]): Set[Int] =
      block match {
        case Nil             => toRemove
        case instr :: instrs =>
          // Check for uses
          val uses = Instruction.args(instr)
          val lastDefPostUses = lastDef -- uses

          // Check for previous defs
          val (nextLastDef, nextToRemove) = Instruction.dest(instr) match {
            case None => (lastDefPostUses, toRemove)
            case Some(Destination(destName, _)) =>
              val updatedLastDef: Map[String, Int] = lastDefPostUses + (destName -> i)
              val updatedToRemove = lastDefPostUses.get(destName) match {
                case Some(prevIndex) => toRemove + prevIndex
                case None            => toRemove
              }
              (updatedLastDef, updatedToRemove)
          }

          findInstrsToRemove(i + 1, instrs, nextLastDef, nextToRemove)
      }

    val indicesToRemove = findInstrsToRemove(0, block, Map.empty, Set.empty)
    block.zipWithIndex.collect {
      case (instr, i) if !indicesToRemove.contains(i) => instr
    }
  }
  def run(basicBlocks: Map[String, List[Instruction]]): Map[String, List[Instruction]] =
    // Functor[Map[String, ?]].map(basicBlocks)(elimInBlock(_))
    basicBlocks.map { case (k, v) => (k, elimInBlock(v)) }
}
