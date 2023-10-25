package bril.analysis.local.extensions

import bril.analysis.local._
import bril.syntax._

// Exploiting the commutativity of `add` and `mul`.
object CommonSubexpressionElimination {
  def preLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr match {
    // Canonicalize arg order for commutative binary expressions
    case BinExpr(op, arg1, arg2) => op match {
      case Add | Mul => BinExpr(op, arg1 min arg2, arg1 max arg2)
      case _ => serExpr
    }
    case _ => serExpr
  }

  def postLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr

  def argConversion(arg: String, table: Table): String = arg

  val extension = Extension(preLookup, postLookup, argConversion)
}
