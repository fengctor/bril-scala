package bril.analysis.local.extensions

import bril.analysis.local._
import bril.syntax._

object CopyPropagation {
  def preLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr

  def postLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr match {
    case UnExpr(Id, arg) =>
      table.idToExpr.get(arg) match {
        case Some(propId @ UnExpr(Id, _)) => propId
        case _                            => serExpr
      }
    case _ => serExpr
  }

  def argConversion(arg: String, table: Table): String = table.idToExpr(table.canonVarToId(arg)) match {
    case UnExpr(Id, idArg) => table.idToCanonVar(idArg)
    case _                 => arg
  }

  val extension = Extension(preLookup, postLookup, argConversion)
}
