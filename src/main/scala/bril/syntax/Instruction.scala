package bril.syntax

import cats.Show
import cats.syntax.all._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

sealed trait Instruction
final case class Label(label: String) extends Instruction
final case class Const(dest: Destination, value: Literal) extends Instruction
final case class Binary(dest: Destination, op: BinaryOperation, arg1: String, arg2: String) extends Instruction
final case class Unary(dest: Destination, op: UnaryOperation, arg: String) extends Instruction
final case class Jmp(label: String) extends Instruction
final case class Br(arg: String, label1: String, label2: String) extends Instruction
final case class Call(optDest: Option[Destination], name: String, args: List[String]) extends Instruction
final case class Ret(optArg: Option[String]) extends Instruction
final case class Print(args: List[String]) extends Instruction
final case object Nop extends Instruction

object Instruction {
  implicit val showInstance: Show[Instruction] = new Show[Instruction] {
    override def show(instr: Instruction): String = instr match {
      case Label(label)                 => s".$label"
      case Const(dest, value)           => s"${dest.show} = const ${value.show}"
      case Binary(dest, op, arg1, arg2) => s"${dest.show} = ${op.show} $arg1 $arg2"
      case Unary(dest, op, arg)         => s"${dest.show} = ${op.show} $arg"
      case Jmp(label)                   => s"jmp .$label"
      case Br(arg, label1, label2)      => s"br $arg .$label1 .$label2"
      case Call(optDest, name, args) =>
        val rhsStr = s"$name ${args.mkString(" ")}"
        optDest match {
          case None       => rhsStr
          case Some(dest) => s"${dest.show} = $rhsStr"
        }
      case Ret(optArg) =>
        optArg match {
          case None      => "ret"
          case Some(arg) => s"ret $arg"
        }
      case Print(args) => s"print ${args.mkString(" ")}"
      case Nop         => "nop"
    }
  }

  implicit val encodeInstance: Encoder[Instruction] = Encoder
    .instance[Instruction] {
      case Label(label) => Json.obj("label" -> label.asJson)
      case Const(Destination(destName, destType), value) =>
        Json.obj(
          "op" -> "const".asJson,
          "dest" -> destName.asJson,
          "type" -> destType.asJson,
          "value" -> value.asJson
        )
      case Binary(Destination(destName, destType), op, arg1, arg2) =>
        Json.obj(
          // TODO: write encode/decode instances for BinaryOperations instead?
          "op" -> op.show.asJson,
          "dest" -> destName.asJson,
          "type" -> destType.asJson,
          "args" -> List(arg1, arg2).asJson
        )
      case Unary(Destination(destName, destType), op, arg) =>
        Json.obj(
          // TODO: write encode/decode instances for UnaryOperations instead?
          "op" -> op.show.asJson,
          "dest" -> destName.asJson,
          "type" -> destType.asJson,
          "args" -> List(arg).asJson
        )
      case Jmp(label) =>
        Json.obj(
          "op" -> "jmp".asJson,
          "labels" -> List(label).asJson
        )
      case Br(arg, label1, label2) =>
        Json.obj(
          "op" -> "br".asJson,
          "args" -> List(arg).asJson,
          "labels" -> List(label1, label2).asJson
        )
      case Call(optDest, name, args) =>
        Json.obj(
          "op" -> "call".asJson,
          "dest" -> optDest.map(_.destName).asJson,
          "type" -> optDest.map(_.destType).asJson,
          "funcs" -> List(name).asJson
        )
      case Ret(optArg) =>
        Json.obj(
          "op" -> "ret".asJson,
          "args" -> optArg.map(List(_)).asJson
        )
      case Print(args) =>
        Json.obj(
          "op" -> "print".asJson,
          "args" -> args.asJson
        )
      case Nop => Json.obj("op" -> "nop".asJson)

    }
    .mapJson(_.dropNullValues)

  val labelDecoder: Decoder[Instruction] = Decoder.instance { c =>
    for {
      label <- c.downField("label").as[String]
    } yield Label(label)
  }

  def failDecodeWith[T](msg: String): Decoder.Result[T] = Left(DecodingFailure(msg, List()))

  def decodeConst(c: HCursor): Decoder.Result[Instruction] = for {
    destName <- c.downField("dest").as[String]
    destType <- c.downField("type").as[Type]
    value <- c.downField("value").as[Literal]
  } yield Const(Destination(destName, destType), value)
  def decodeBinary(op: BinaryOperation, c: HCursor): Decoder.Result[Instruction] = for {
    destName <- c.downField("dest").as[String]
    destType <- c.downField("type").as[Type]
    argPair <- c.downField("args").as[List[String]].flatMap {
      case List(v1, v2) => Right((v1, v2))
      case vs           => failDecodeWith(s"Invalid number of arguments for ${op.show}: ${vs.show}")
    }
    (arg1, arg2) = argPair
  } yield Binary(Destination(destName, destType), op, arg1, arg2)
  def decodeUnary(op: UnaryOperation, c: HCursor): Decoder.Result[Instruction] = for {
    destName <- c.downField("dest").as[String]
    destType <- c.downField("type").as[Type]
    arg <- c.downField("args").as[List[String]].flatMap {
      case List(v) => Right(v)
      case vs      => failDecodeWith(s"Invalid number of arguments for ${op.show}: ${vs.show}")
    }
  } yield Unary(Destination(destName, destType), op, arg)
  def decodeJmp(c: HCursor): Decoder.Result[Instruction] = for {
    label <- c.downField("labels").as[List[String]].flatMap {
      case List(l) => Right(l)
      case vs      => failDecodeWith(s"Invalid number of labels for jmp: ${vs.show}")
    }
  } yield Jmp(label)
  def decodeBr(c: HCursor): Decoder.Result[Instruction] = for {
    arg <- c.downField("args").as[List[String]].flatMap {
      case List(v) => Right(v)
      case vs      => failDecodeWith(s"Invalid number of arguments for br: ${vs.show}")
    }
    labelPair <- c.downField("labels").as[List[String]].flatMap {
      case List(l1, l2) => Right((l1, l2))
      case vs           => failDecodeWith(s"Invalid number of labels for br: ${vs.show}")
    }
    (label1, label2) = labelPair
  } yield Br(arg, label1, label2)
  def decodeCall(c: HCursor): Decoder.Result[Instruction] = for {
    optDestName <- c.downField("dest").as[Option[String]]
    optDestType <- c.downField("type").as[Option[Type]]
    optDest = (optDestName, optDestType).mapN(Destination(_, _))
    name <- c.downField("funcs").as[List[String]].flatMap {
      case List(s) => Right(s)
      case vs      => failDecodeWith(s"Invalid number of function names for call: ${vs.show}")
    }
    args <- c.downField("args").as[List[String]]
  } yield Call(optDest, name, args)
  def decodeRet(c: HCursor): Decoder.Result[Instruction] = for {
    // TODO: match on single arg
    optArg <- c.downField("args").as[Option[List[String]]].flatMap {
      case None          => Right(None)
      case Some(List(v)) => Right(Some(v))
      case Some(vs)      => failDecodeWith(s"Invalid number of arguments for ret: ${vs.show}")
    }
  } yield Ret(optArg)
  def decodePrint(c: HCursor): Decoder.Result[Instruction] = for {
    args <- c.downField("args").as[List[String]]
  } yield Print(args)

  val opInstructionDecoder: Decoder[Instruction] = Decoder.instance { c =>
    for {
      op <- c.downField("op").as[String]
      result <- op match {
        case "const" => decodeConst(c)
        case "add" | "mul" | "sub" | "div" | "eq" | "lt" | "gt" | "le" | "ge" | "and" | "or" =>
          decodeBinary(BinaryOperation.fromString(op), c)
        case "not" | "id" => decodeUnary(UnaryOperation.fromString(op), c)
        case "jmp"        => decodeJmp(c)
        case "br"         => decodeBr(c)
        case "call"       => decodeCall(c)
        case "ret"        => decodeRet(c)
        case "print"      => decodePrint(c)
        case "nop"        => Right(Nop)
        case _            => Left(DecodingFailure(s"Not a valid operation: $op", List()))
      }
    } yield result
  }
  implicit val decoderInstance: Decoder[Instruction] = labelDecoder or opInstructionDecoder

  def dest(instr: Instruction): Option[Destination] = instr match {
    case Const(dest, _)        => Some(dest)
    case Binary(dest, _, _, _) => Some(dest)
    case Unary(dest, _, _)     => Some(dest)
    case Call(optDest, _, _)   => optDest
    case _                     => None
  }

  def args(instr: Instruction): List[String] = instr match {
    case Binary(_, _, arg1, arg2) => List(arg1, arg2)
    case Unary(_, _, arg)         => List(arg)
    case Br(arg, _, _)            => List(arg)
    case Call(_, _, args)         => args
    case Ret(optArg)              => optArg.toList
    case Print(args)              => args
    case _                        => Nil
  }

  def mapArgs(instr: Instruction)(f: String => String): Instruction = instr match {
    case Binary(dest, op, arg1, arg2) => Binary(dest, op, f(arg1), f(arg2))
    case Unary(dest, op, arg)         => Unary(dest, op, f(arg))
    case Br(arg, label1, label2)      => Br(f(arg), label1, label2)
    case Call(optDest, name, args)    => Call(optDest, name, args.map(f))
    case Ret(optArg)                  => Ret(optArg.map(f))
    case Print(args)                  => Print(args.map(f))
    case _                            => instr
  }
}
