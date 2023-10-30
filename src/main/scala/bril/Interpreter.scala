package bril

import bril.analysis.Cfg
import bril.syntax._
import cats.syntax.all._

object Interpreter {
  case class State(
    env: Map[String, Literal],
    cfg: Cfg,
    blockName: String,
    functionCfgs: Map[String, (List[Destination], Cfg)]
  )
  def updatedStateEnv(state: State, bindings: (String, Literal)*): State =
    state.copy(env = bindings.foldLeft(state.env) { (acc, cur) =>
      val (variable, value) = cur
      acc.updated(variable, value)
    })

  sealed trait BlockResult
  final case class NextBlock(nextBlockName: String) extends BlockResult
  final case class Finish(optResult: Option[Literal]) extends BlockResult

  def evalConst(state: State, destName: String, value: Literal): State = updatedStateEnv(state, destName -> value)
  def evalBinary(state: State, destName: String, op: BinaryOperation, arg1: String, arg2: String): State = {
    val arg1Value = state.env(arg1)
    val arg2Value = state.env(arg2)
    val resultValue = (op, arg1Value, arg2Value) match {
      case (Add, IntLit(x), IntLit(y))   => IntLit(x + y)
      case (Mul, IntLit(x), IntLit(y))   => IntLit(x * y)
      case (Sub, IntLit(x), IntLit(y))   => IntLit(x - y)
      case (Div, IntLit(x), IntLit(y))   => IntLit(x / y)
      case (Eq, IntLit(x), IntLit(y))    => BoolLit(x == y)
      case (Lt, IntLit(x), IntLit(y))    => BoolLit(x < y)
      case (Gt, IntLit(x), IntLit(y))    => BoolLit(x > y)
      case (Le, IntLit(x), IntLit(y))    => BoolLit(x <= y)
      case (Ge, IntLit(x), IntLit(y))    => BoolLit(x >= y)
      case (And, BoolLit(u), BoolLit(v)) => BoolLit(u && v)
      case (Or, BoolLit(u), BoolLit(v))  => BoolLit(u || v)
      case _ => throw new Exception(s"Invalid args for operation: $op $arg1Value $arg2Value")
    }
    updatedStateEnv(state, destName -> resultValue)
  }
  def evalUnary(state: State, destName: String, op: UnaryOperation, arg: String): State = {
    val argValue = state.env(arg)
    val resultValue = (op, argValue) match {
      case (Not, BoolLit(u)) => BoolLit(!u)
      case (Id, IntLit(x))   => IntLit(x)
      case _                 => throw new Exception(s"Invalid args for operation: $op $argValue")
    }
    updatedStateEnv(state, destName -> resultValue)
  }
  // will call evalFunction
  def evalCall(state: State, optDestName: Option[String], name: String, args: List[String]): State = {
    val argValues = args.map(state.env(_))
    val optResult = evalFunction(state, name, argValues)
    optDestName match {
      case None => state
      case Some(destName) =>
        optResult match {
          case None              => throw new Exception(s"Call with destination $destName must return a value")
          case Some(resultValue) => updatedStateEnv(state, destName -> resultValue)
        }
    }
  }
  def evalPrint(state: State, args: List[String]): State = {
    args.foreach { arg =>
      println(state.env(arg).show)
    }
    state
  }
  def evalInstruction(state: State, instr: Instruction): (State, Option[BlockResult]) = instr match {
    case Label(label) => throw new Exception(s"Should not encounter label at this point: $label")
    case Const(Destination(destName, _), value)           => (evalConst(state, destName, value), None)
    case Binary(Destination(destName, _), op, arg1, arg2) => (evalBinary(state, destName, op, arg1, arg2), None)
    case Unary(Destination(destName, _), op, arg)         => (evalUnary(state, destName, op, arg), None)
    case Jmp(label) =>
      state.cfg.edges(state.blockName) match {
        case List(nextBlockName) => (state.copy(blockName = nextBlockName), Some(NextBlock(nextBlockName)))
        case _ =>
          throw new Exception(s"Malformed CFG: block ${state.blockName} ending with jmp should only have 1 neighbour")
      }
    case Br(arg, label1, label2) =>
      state.env(arg) match {
        case BoolLit(true)  => (state, Some(NextBlock(state.cfg.edges(state.blockName)(0))))
        case BoolLit(false) => (state, Some(NextBlock(state.cfg.edges(state.blockName)(1))))
        case argValue       => throw new Exception(s"Invalid branch argument: $arg -> $argValue")
      }
    case Call(optDest, name, args) => (evalCall(state, optDest.map(_.destName), name, args), None)
    case Ret(optArg)               => (state, Some(Finish(optArg.map(state.env(_)))))
    case Print(args)               => (evalPrint(state, args), None)
    case Nop                       => (state, None)
  }

  // Returns the final state and the next step after evaluating the basic block
  def evalBasicBlock(state: State, instrs: List[Instruction]): (State, Option[BlockResult]) = instrs match {
    case Nil         => throw new Exception("Invalid empty block") // shouldn't happen
    case List(instr) => evalInstruction(state, instr)
    case i :: is =>
      val (updatedState, _) = evalInstruction(state, i)
      evalBasicBlock(updatedState, is)
  }

  // Evalutes state.cfg
  // TODO: evaluating a basic block needs to return an optional value for return value
  def evalCfg(state: State): Option[Literal] = {
    val (updatedState, optNextBlockName) = evalBasicBlock(state, state.cfg.basicBlocks(state.blockName))
    optNextBlockName match {
      case None => None // TODO return value here
      case Some(nextStep) =>
        nextStep match {
          case NextBlock(nextBlockName) =>
            val nextState = updatedState.copy(blockName = nextBlockName)
            evalCfg(nextState)
          case Finish(optResult) => optResult
        }
    }
  }

  def evalFunction(state: State, name: String, argValues: List[Literal]): Option[Literal] = {
    val (argDests, initCfg) = state.functionCfgs(name)
    val argNames = argDests.map(_.destName)
    val initEnv = argNames.zip(argValues).toMap
    val initState = state.copy(env = initEnv, cfg = initCfg, blockName = initCfg.entryBlock)
    evalCfg(initState)
  }

  def run(
    main: String,
    argValues: List[Literal],
    functionCfgs: Map[String, (List[Destination], Cfg)]
  ): Option[Literal] = {
    val (argDests, initCfg) = functionCfgs(main)
    val initState = State(Map.empty, initCfg, initCfg.entryBlock, functionCfgs)
    evalFunction(initState, main, argValues)
  }
}
