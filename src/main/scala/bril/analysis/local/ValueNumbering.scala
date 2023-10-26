package bril.analysis.local

import bril.syntax._
import bril.analysis.local.extensions.Extension
import cats.syntax.all._

sealed trait SerializedExpression
final case class ConstExpr(value: Literal) extends SerializedExpression
final case class BinExpr(op: BinaryOperation, arg1: Int, arg2: Int) extends SerializedExpression
final case class UnExpr(op: UnaryOperation, arg: Int) extends SerializedExpression

// Table with rows associating
// (value ID, serialized expression, canonical variable)
case class Table(
  idToExpr: Map[Int, SerializedExpression],
  idToCanonVar: Map[Int, String],
  exprToId: Map[SerializedExpression, Int],
  canonVarToId: Map[String, Int]
)

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
    Table(
      table.idToExpr + (id -> serExpr),
      table.idToCanonVar + (id -> canonVar),
      table.exprToId + (serExpr -> id),
      table.canonVarToId + (canonVar -> id)
    )

  // ext: a set of functions to interpret a serialized expression as another one for creating other optimizations
  def runWithExtension(ext: Extension)(block: List[Instruction]): List[Instruction] = {
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
      accRevResult: List[Instruction]
    ): List[Instruction] = block match {
      case Nil => accRevResult.reverse
      case instr :: instrs =>
        val (initTable, initVarToId) = initArgs(table, varToId, Instruction.args(instr))
        val (nextTable, nextVarToId, resultInstr) = serialize(instr, initVarToId) match {
          case None =>
            // Remap args to their canonical variables
            val remappedInstr = Instruction.mapArgs(instr)(arg => initTable.idToCanonVar(initVarToId(arg)))
            (initTable, initVarToId, remappedInstr)
          case Some((dest, serExpr)) =>
            val preLookupSerExpr = ext.preLookup(serExpr, initTable)
            initTable.exprToId.get(preLookupSerExpr) match {
              case None =>
                val postLookupSerExpr = ext.postLookup(preLookupSerExpr, initTable)
                // If instruction is overwritten later, use a fresh variable as the canonical var
                // and when then instruction is reconstructed, use it as the destination variable
                val canonDestName =
                  if (overwritten.contains(instr)) freshVariable()
                  else dest.destName
                val updatedDest = dest.copy(destName = canonDestName)
                val updatedTable = addRow(initTable, curId, postLookupSerExpr, canonDestName)
                // Still use original variable name for environment since usages will only
                // possibly refer to it, not the possibly fresh name
                val updatedVarToId = initVarToId + (dest.destName -> curId)
                curId += 1
                (updatedTable, updatedVarToId, deserialize(updatedDest, postLookupSerExpr, updatedTable.idToCanonVar))
              case Some(existingId) =>
                val updatedVarToId = initVarToId + (dest.destName -> existingId)
                val postLookupSerExpr = ext.postLookup(UnExpr(Id, existingId), initTable)
                (initTable, updatedVarToId, deserialize(dest, postLookupSerExpr, initTable.idToCanonVar))
            }
        }
        val convertedInstr = Instruction.mapArgs(resultInstr)(arg => ext.argConversion(arg, nextTable))
        numberValues(instrs, nextTable, nextVarToId, convertedInstr :: accRevResult)
    }

    // Reset IDs for each block
    curId = 0
    numberValues(block, Table(Map.empty, Map.empty, Map.empty, Map.empty), Map.empty, Nil)
  }
}
