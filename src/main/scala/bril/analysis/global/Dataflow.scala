package bril.analysis.global

import bril.syntax._
import bril.analysis.Cfg
import cats.Show
import cats.syntax.all._
import scala.collection.immutable.Queue

object Dataflow {
  sealed trait Direction
  final case object Forwards extends Direction
  final case object Backwards extends Direction

  case class Results[T](in: Map[String, T], out: Map[String, T])
  object Results {
    implicit def showInstance[T](implicit showT: Show[T]): Show[Results[T]] = new Show[Results[T]] {
      override def show(res: Results[T]): String = {
        // List the in and out set for each block
        val allBlockNames = res.in.keys
        allBlockNames
          .map { blockName =>
            s"""$blockName
               |in:  ${res.in(blockName).show}
               |out: ${res.out(blockName).show}
               """.stripMargin
          }
          .mkString("\n")
      }
    }
  }

  // TODO: should transfer take the old "out" for the current block and determine whether it changed for efficiency?
  def run[T](direction: Direction, init: T, merge: List[T] => T, transfer: (List[Instruction], T) => T)(
    cfg: Cfg
  ): Results[T] = {
    val (preds, succs) = {
      val originalPreds = Cfg.predecessors(cfg)
      val originalSuccs = Cfg.successors(cfg)
      direction match {
        case Forwards  => (originalPreds, originalSuccs)
        case Backwards => (originalSuccs, originalPreds)
      }
    }

    def go(in: Map[String, T], out: Map[String, T], worklist: Queue[String]): Results[T] =
      if (worklist.isEmpty) direction match {
        case Forwards  => Results(in, out)
        case Backwards => Results(out, in)
      }
      else {
        val (curBlockName, restWorklist) = worklist.dequeue
        val curBlock = cfg.basicBlocks(curBlockName)
        val newCurIn = merge(preds(curBlockName).map(out(_)))
        val newCurOut = transfer(curBlock, newCurIn)
        val newWorklist =
          if (newCurOut != out(curBlockName)) restWorklist ++ succs(curBlockName)
          else restWorklist
        go(in.updated(curBlockName, newCurIn), out.updated(curBlockName, newCurOut), newWorklist)
      }

    val allBlockNames = cfg.basicBlocks.keys.toSeq
    val initIn = Map(cfg.entryBlock -> init)
    val initOut = allBlockNames.map(_ -> init).toMap
    val initWorklist = Queue(allBlockNames: _*)
    go(initIn, initOut, initWorklist)
  }
}
