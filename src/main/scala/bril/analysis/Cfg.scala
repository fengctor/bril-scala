package bril.analysis

import bril.syntax._
import cats.Show
import cats.syntax.all._

case class Cfg(basicBlocks: Map[String, List[Instruction]], edges: Map[String, List[String]], entryBlock: String)

object Cfg {
  implicit val showInstance: Show[Cfg] = new Show[Cfg] {
    // TODO: consider graph traversal to show blocks in a better order
    override def show(cfg: Cfg): String = {
      val blocksStr = cfg.basicBlocks.toList
        .map { case (blockName, blockInstrs) =>
          s"$blockName:\n${blockInstrs.map(instr => s"  ${instr.show}").mkString("\n")}"
        }
        .mkString("\n\n")
      val edgesStr = cfg.edges.toList
        .map { case (from, tos) =>
          s"$from -> ${tos.mkString(", ")}"
        }
        .mkString("\n")
      s"Entry block: ${cfg.entryBlock}\n\n$blocksStr\n\n$edgesStr"
    }
  }

  def fromInstructions(functionName: String, instrs: List[Instruction]): Cfg = {
    def blockName(baseName: String): String = s"$functionName.$baseName"
    def numberedBlockName(n: Int): String = blockName(s"__b$n")
    def labelledBlockName(label: String): String = blockName(label)
    def go(
      instrs: List[Instruction],
      curName: String,
      revCurBlock: List[Instruction],
      accNamedBlocks: List[(String, List[Instruction])],
      accEdges: List[(String, List[String])]
    ): Cfg = instrs match {
      case Nil =>
        val allNamedBlocks =
          if (revCurBlock.isEmpty) accNamedBlocks else (curName, revCurBlock.reverse) :: accNamedBlocks
        Cfg(allNamedBlocks.toMap, accEdges.toMap, allNamedBlocks.last._1)
      case curInstr :: restInstrs =>
        def goOnTerminator(outBlockNames: List[String]): Cfg = {
          val curBlock = (curInstr :: revCurBlock).reverse
          val updatedNamedBlocks = (curName, curBlock) :: accNamedBlocks
          val updatedEdges = if (outBlockNames.isEmpty) accEdges else (curName, outBlockNames) :: accEdges
          go(restInstrs, numberedBlockName(updatedNamedBlocks.length), Nil, updatedNamedBlocks, updatedEdges)
        }
        curInstr match {
          case Label(label) =>
            val labelBlockName = labelledBlockName(label)
            if (revCurBlock.isEmpty) go(restInstrs, labelBlockName, Nil, accNamedBlocks, accEdges)
            else {
              val curBlock = revCurBlock.reverse
              val updatedNamedBlocks = (curName, curBlock) :: accNamedBlocks
              val outBlockNames = List(labelBlockName)
              val updatedEdges = (curName, outBlockNames) :: accEdges
              go(restInstrs, labelBlockName, Nil, updatedNamedBlocks, updatedEdges)
            }
          case Jmp(label)            => goOnTerminator(List(labelledBlockName(label)))
          case Br(_, label1, label2) => goOnTerminator(List(labelledBlockName(label1), labelledBlockName(label2)))
          case Ret(_)                => goOnTerminator(Nil)
          case _                     => go(restInstrs, curName, curInstr :: revCurBlock, accNamedBlocks, accEdges)
        }
    }
    go(instrs, numberedBlockName(0), Nil, Nil, Nil)
  }

  def successors(cfg: Cfg): Map[String, List[String]] = {
    val defaults = cfg.basicBlocks.keys.map(_ -> Nil).toMap
    defaults ++ cfg.edges
  }
  def predecessors(cfg: Cfg): Map[String, List[String]] = {
    val defaults = cfg.basicBlocks.keys.map(_ -> Nil).toMap
    defaults ++ cfg.edges.toList
      // reverse edges
      .flatMap { case (from, tos) => tos.map(to => (to, from)) }
      // rebuild adjacency lists
      .foldLeft(Map.empty[String, List[String]]) { (acc, cur) =>
        val (from, to) = cur
        acc.updated(from, to :: acc.getOrElse(from, Nil))
      }
  }
}
