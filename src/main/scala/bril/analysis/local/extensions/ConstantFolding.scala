package bril.analysis.local.extensions

import bril.analysis.local._
import bril.syntax._

object ConstantFolding {
  def preLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr

  def postLookup(serExpr: SerializedExpression, table: Table): SerializedExpression = serExpr match {
    case BinExpr(op, arg1, arg2) =>
      (op, table.idToExpr(arg1), table.idToExpr(arg2)) match {
        case (Add, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))   => ConstExpr(IntLit(x + y))
        case (Sub, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))   => ConstExpr(IntLit(x - y))
        case (Mul, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))   => ConstExpr(IntLit(x * y))
        case (Div, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))   => ConstExpr(IntLit(x / y))
        case (Eq, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))    => ConstExpr(BoolLit(x == y))
        case (Lt, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))    => ConstExpr(BoolLit(x < y))
        case (Gt, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))    => ConstExpr(BoolLit(x > y))
        case (Le, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))    => ConstExpr(BoolLit(x <= y))
        case (Ge, ConstExpr(IntLit(x)), ConstExpr(IntLit(y)))    => ConstExpr(BoolLit(x >= y))
        case (And, ConstExpr(BoolLit(u)), ConstExpr(BoolLit(v))) => ConstExpr(BoolLit(u && v))
        case (Or, ConstExpr(BoolLit(u)), ConstExpr(BoolLit(v)))  => ConstExpr(BoolLit(u || v))
        case _                                                   => serExpr
      }
    case UnExpr(Not, arg) =>
      table.idToExpr(arg) match {
        case ConstExpr(BoolLit(b)) => ConstExpr(BoolLit(!b))
        case _                     => serExpr
      }
    case _ => serExpr
  }

  def argConversion(arg: String, table: Table): String = arg

  val extension = Extension(preLookup, postLookup, argConversion)
}
