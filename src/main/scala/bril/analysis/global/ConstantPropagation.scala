package bril.analysis.global

import bril.syntax._
import bril.analysis.Cfg
import cats.Show
import cats.syntax.all._

object ConstantPropagation {
  sealed trait ConstDetermination
  final case class KnownConst(value: Literal) extends ConstDetermination
  final case object NonConst extends ConstDetermination
  final case object Unknown extends ConstDetermination

  object ConstDetermination {
    implicit val showInstance: Show[ConstDetermination] = new Show[ConstDetermination] {
      override def show(constDet: ConstDetermination): String = constDet match {
        case KnownConst(value) => value.show
        case NonConst          => "[X]"
        case Unknown           => "???"
      }
    }
  }

  def init(f: Function): Map[String, ConstDetermination] = f.args.map { case Destination(destName, _) =>
    destName -> NonConst
  }.toMap

  def combineDeterminations(cd1: ConstDetermination, cd2: ConstDetermination): ConstDetermination = (cd1, cd2) match {
    case (Unknown, _)                     => cd2
    case (_, Unknown)                     => cd1
    case (NonConst, _)                    => NonConst
    case (_, NonConst)                    => NonConst
    case (KnownConst(v1), KnownConst(v2)) => if (v1 == v2) cd1 else NonConst
  }

  def combineValuations(
    m1: Map[String, ConstDetermination],
    m2: Map[String, ConstDetermination]
  ): Map[String, ConstDetermination] =
    m2.foldLeft(m1) { (acc, cur) =>
      val (varName, constDet) = cur
      acc.updated(varName, combineDeterminations(acc.getOrElse(varName, Unknown), constDet))
    }

  def merge(predOuts: List[Map[String, ConstDetermination]]): Map[String, ConstDetermination] =
    predOuts.foldLeft(Map.empty[String, ConstDetermination])(combineValuations)

  def transfer(block: List[Instruction], curIn: Map[String, ConstDetermination]): Map[String, ConstDetermination] =
    block.foldLeft(curIn) { (accValuation, curInstr) =>
      curInstr match {
        case Const(Destination(destName, _), value) => accValuation.updated(destName, KnownConst(value))
        case Binary(Destination(destName, _), op, arg1, arg2) =>
          val curConstDet = (accValuation.getOrElse(arg1, Unknown), accValuation.getOrElse(arg2, Unknown)) match {
            case (NonConst, _) | (_, NonConst) => NonConst
            case (Unknown, _) | (_, Unknown)   => Unknown
            case (KnownConst(value1), KnownConst(value2)) =>
              (op, value1, value2) match {
                case (Add, IntLit(n1), IntLit(n2))   => KnownConst(IntLit(n1 + n2))
                case (Mul, IntLit(n1), IntLit(n2))   => KnownConst(IntLit(n1 * n2))
                case (Sub, IntLit(n1), IntLit(n2))   => KnownConst(IntLit(n1 - n2))
                case (Div, IntLit(n1), IntLit(n2))   => KnownConst(IntLit(n1 / n2))
                case (Eq, IntLit(n1), IntLit(n2))    => KnownConst(BoolLit(n1 == n2))
                case (Lt, IntLit(n1), IntLit(n2))    => KnownConst(BoolLit(n1 < n2))
                case (Gt, IntLit(n1), IntLit(n2))    => KnownConst(BoolLit(n1 > n2))
                case (Le, IntLit(n1), IntLit(n2))    => KnownConst(BoolLit(n1 <= n2))
                case (Ge, IntLit(n1), IntLit(n2))    => KnownConst(BoolLit(n1 >= n2))
                case (And, BoolLit(b1), BoolLit(b2)) => KnownConst(BoolLit(b1 && b2))
                case (Or, BoolLit(b1), BoolLit(b2))  => KnownConst(BoolLit(b1 || b2))
                case _ => throw new Exception(s"Invalid types for arguments: ${curInstr.show}")
              }
          }
          accValuation.updated(destName, curConstDet)
        case Unary(Destination(destName, _), op, arg) =>
          val curConstDet = accValuation.getOrElse(arg, Unknown) match {
            case NonConst => NonConst
            case Unknown  => Unknown
            case KnownConst(value) =>
              (op, value) match {
                case (Not, BoolLit(b)) => KnownConst(BoolLit(!b))
                case (Id, _)           => KnownConst(value)
                case _                 => throw new Exception(s"Invalid types for arguments: ${curInstr.show}")
              }
          }
          accValuation.updated(destName, curConstDet)
        case Call(Some(Destination(destName, _)), _, _) => accValuation.updated(destName, Unknown)
        case _                                          => accValuation
      }
    }

  def run(f: Function)(cfg: Cfg): Dataflow.Results[Map[String, ConstDetermination]] =
    Dataflow.run(Dataflow.Forwards, init(f), merge, transfer)(cfg)
}
