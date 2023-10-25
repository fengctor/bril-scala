package bril.analysis.local.extensions

import bril.analysis.local._
import bril.syntax._

object ConstantPropagation {
  def preLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr

  def postLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr match {
    case UnExpr(Id, arg) =>
      table.idToExpr.get(arg) match {
        case Some(propConst @ ConstExpr(_)) => propConst
        case _                              => serExpr
      }
    case _ => serExpr
  }

  def argConversion(arg: String, table: Table): String = arg

  val extension = Extension(preLookup, postLookup, argConversion)
}
