package bril.analysis.local

import bril.syntax._
import cats.syntax.all._

sealed trait SerializedExpression
final case class ConstExpr(value: Literal) extends SerializedExpression
final case class BinExpr(op: BinaryOperation, arg1: Int, arg2: Int) extends SerializedExpression
final case class UnExpr(op: UnaryOperation, arg: Int) extends SerializedExpression

// Table with rows associating
// (value ID, serialized expression, canonical variable)
case class Table(exprToId: Map[SerializedExpression, Int], idToCanonVar: Map[Int, String])

object ValueNumbering {
  // Used to generate fresh variables
  var freshCounter: Int = 0
  def freshVariable(): String = {
    val result = s"__fresh$freshCounter"
    freshCounter += 1
    result
  }

  // Used to generate fresh value IDs
  var curId: Int = 0

  def serialize(instr: Instruction, varToId: Map[String, Int]): Option[(Destination, SerializedExpression)] =
    instr match {
      case Const(dest, value)           => Some((dest, ConstExpr(value)))
      case Binary(dest, op, arg1, arg2) => Some((dest, BinExpr(op, varToId(arg1), varToId(arg2))))
      case Unary(dest, op, arg)         => Some((dest, UnExpr(op, varToId(arg))))
      case _                            => None
    }

  def deserialize(dest: Destination, serExpr: SerializedExpression, idToVar: Map[Int, String]): Instruction =
    serExpr match {
      case ConstExpr(value)        => Const(dest, value)
      case BinExpr(op, arg1, arg2) => Binary(dest, op, idToVar(arg1), idToVar(arg2))
      case UnExpr(op, arg)         => Unary(dest, op, idToVar(arg))
    }

  def addRow(table: Table, id: Int, serExpr: SerializedExpression, canonVar: String): Table =
    Table(table.exprToId + (serExpr -> id), table.idToCanonVar + (id -> canonVar))

  // ext: a function to interpret a serialized expression as another one for other optimizations that need this
  def runWithExtension(
    ext: (SerializedExpression, Table) => SerializedExpression
  )(block: List[Instruction]): List[Instruction] = {
    // Returns the set of Instructions that are overwritten later in the block
    def findOverwritten(
      block: List[Instruction],
      varToDef: Map[String, Instruction],
      accResult: Set[Instruction]
    ): Set[Instruction] = block match {
      case Nil => accResult
      case instr :: instrs =>
        Instruction.dest(instr) match {
          case None => findOverwritten(instrs, varToDef, accResult)
          case Some(Destination(destName, _)) =>
            varToDef.get(destName) match {
              case None           => findOverwritten(instrs, varToDef + (destName -> instr), accResult)
              case Some(oldInstr) => findOverwritten(instrs, varToDef + (destName -> instr), accResult + oldInstr)
            }
        }
    }
    val overwritten = findOverwritten(block, Map.empty, Set.empty)

    // Give arguments that don't exist in the varToId mapping a fresh value
    def initArgs(table: Table, varToId: Map[String, Int], args: List[String]): (Table, Map[String, Int]) =
      args.foldLeft((table, varToId)) { (acc, curArg) =>
        val (accTable, accVarToId) = acc
        if (accVarToId.contains(curArg)) acc
        else {
          val argId = curId
          curId += 1
          val dummyExpr = UnExpr(Id, argId)
          (addRow(accTable, argId, dummyExpr, curArg), varToId + (curArg -> argId))
        }
      }

    // Produces the block with serialized expressions in place of instructions where applicable
    // paired with the final mapping from ID to canonical variables found
    def numberValues(
      block: List[Instruction],
      table: Table,
      varToId: Map[String, Int],
      accRevResult: List[Either[Instruction, (Destination, SerializedExpression)]]
    ): (List[Either[Instruction, (Destination, SerializedExpression)]], Map[Int, String]) = block match {
      case Nil => (accRevResult.reverse, table.idToCanonVar)
      case instr :: instrs =>
        val (initTable, initVarToId) = initArgs(table, varToId, Instruction.args(instr))
        serialize(instr, initVarToId) match {
          case None => numberValues(instrs, initTable, initVarToId, Left(instr) :: accRevResult)
          case Some((dest, serExpr)) =>
            val convertedSerExpr = ext(serExpr, table)
            initTable.exprToId.get(convertedSerExpr) match {
              case None =>
                // If instruction is overwritten later, use a fresh variable as the canonical var
                // and when then instruction is reconstructed, use it as the destination variable
                val canonDestName =
                  if (overwritten.contains(instr)) freshVariable()
                  else dest.destName
                val updatedDest = dest.copy(destName = canonDestName)
                val updatedTable = addRow(initTable, curId, convertedSerExpr, canonDestName)
                // Still use original variable name for environment since usages will only
                // possibly refer to it, not the possibly fresh name
                val updatedVarToId = initVarToId + (dest.destName -> curId)
                curId += 1
                numberValues(
                  instrs,
                  updatedTable,
                  updatedVarToId,
                  Right((updatedDest, convertedSerExpr)) :: accRevResult
                )
              case Some(existingId) =>
                val updatedVarToId = initVarToId + (dest.destName -> existingId)
                val replacedSerExpr = UnExpr(Id, existingId)
                numberValues(
                  instrs,
                  initTable,
                  updatedVarToId,
                  Right((dest, replacedSerExpr)) :: accRevResult
                )
            }
        }
    }

    def backToInstruction(
      result: Either[Instruction, (Destination, SerializedExpression)],
      idToVar: Map[Int, String]
    ): Instruction = result match {
      case Left(instr)            => instr
      case Right((dest, serExpr)) => deserialize(dest, serExpr, idToVar)
    }

    // Reset IDs for each block
    curId = 0
    val (resultList, idToVar) = numberValues(block, Table(Map.empty, Map.empty), Map.empty, Nil)
    resultList.map(backToInstruction(_, idToVar))
  }
}
