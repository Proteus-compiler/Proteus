package Compiler

sealed trait ReturnAnalysis
case object NoReturn extends ReturnAnalysis
case object MaybeReturns extends ReturnAnalysis
case object DefinitelyReturns extends ReturnAnalysis

case class FormalFuncArgs(theVar: VarName, typ: Type)
case class DefFunc(returnType: Type, name: FuncName, args: Seq[FormalFuncArgs], body: Statement)

trait Expression {
  var typ: Type = null
}

object Typechecker {

  def makeFunctionMap(prog: Program): Map[FuncName, (Seq[Type], Type)] = {
    prog.defFuncs.foldLeft(Map[FuncName, (Seq[Type], Type)]())((accum, cur) => {
      val DefFunc(returnType, name, args, _) = cur
      if (accum.contains(name)) {
        throw TypeErrorException("Duplicate function name: " + name)
      }
      accum + (name -> (args.map(_.typ), returnType))
    })
  }

  def typecheckProgram(prog: Program): Unit = {
    val functionMapping = makeFunctionMap(prog)
    prog.defFuncs.foreach(func => typecheckFunc(func, functionMapping))
  }

  def makeEnv(args: Seq[FormalFuncArgs], scopeLevel: Int): Map[VarName, (Type, Int)] = {
    args.foldLeft(Map[VarName, (Type, Int)]())((accum, cur) => {
      if (accum.contains(cur.theVar)) {
        throw TypeErrorException("Duplicate variable name: " + cur.theVar)
      }
      accum + (cur.theVar -> (cur.typ, scopeLevel))
    })
  }

  def typecheckFunc(func: DefFunc, funcs: Map[FuncName, (Seq[Type], Type)]): Unit = {
    val (_, returnAnalysis) =
      typecheck(
        func.body,
        0,
        makeEnv(func.args, 0),
        funcs,
        func.returnType)

    returnAnalysis match {
      case DefinitelyReturns => ()
      case _ =>
        throw TypeErrorException("Function might not return: " + func)
    }
  }

  def typecheckStmts(
    stmts: Seq[Statement],
    scopeLevel: Int,
    env: Map[VarName, (Type, Int)],
    funcs: Map[FuncName, (Seq[Type], Type)],
    returnType: Type
  ): (Map[VarName, (Type, Int)], ReturnAnalysis) = {

    stmts.foldLeft((env, NoReturn: ReturnAnalysis)) { case ((curEnv, curReturn), curStmt) =>
      val (nextEnv, stmtReturn) = typecheck(curStmt, scopeLevel, curEnv, funcs, returnType)
      val nextReturn: ReturnAnalysis = (curReturn, stmtReturn) match {
        case (NoReturn, DefinitelyReturns) => DefinitelyReturns
        case (NoReturn, MaybeReturns) => MaybeReturns
        case (NoReturn, NoReturn) => NoReturn
        case (MaybeReturns, NoReturn) => MaybeReturns
        case (MaybeReturns, MaybeReturns) => MaybeReturns
        case (MaybeReturns, DefinitelyReturns) => DefinitelyReturns
        case (DefinitelyReturns, _) =>
          throw TypeErrorException("Dead code: " + curStmt)
      }
      (nextEnv, nextReturn)
    }
  }

  def assertTypesSame(expected: Type, received: Type): Unit = {
    if (expected != received) {
      throw TypeErrorException(s"Expected type: $expected; received type: $received")
    }
  }

  def typecheck(
    stmt: Statement,
    scopeLevel: Int,
    env: Map[VarName, (Type, Int)],
    funcs: Map[FuncName, (Seq[Type], Type)],
    returnType: Type
  ): (Map[VarName, (Type, Int)], ReturnAnalysis) = {
    stmt match {
      case ReturnStmt(exp) =>
        assertTypesSame(returnType, typeof(exp, env, funcs))
        (env, DefinitelyReturns)

      case PrintlnStmt(exp) =>
        typeof(exp, env, funcs)
        (env, NoReturn)

      case DecStmt(expectedType, theVar, initializer) =>
        val receivedType = typeof(initializer, env, funcs)
        assertTypesSame(expectedType, receivedType)
        env.get(theVar) match {
          case Some((_, `scopeLevel`)) =>
            throw TypeErrorException("Name in same scope: " + theVar)
          case _ =>
            (env + (theVar -> (receivedType, scopeLevel)), NoReturn)
        }

      case Block(stmts) =>
        val (_, returnAnalysis) =
          typecheckStmts(stmts, scopeLevel + 1, env, funcs, returnType)
        (env, returnAnalysis)
    }
  }

  def typeof(
    exp: Expression,
    env: Map[VarName, (Type, Int)],
    funcs: Map[FuncName, (Seq[Type], Type)]
  ): Type = {
    val expType: Type = exp match {
      case IntLiteralExpression(_) => IntType
      case BoolLiteralExpression(_) => BoolType

      case VarExpression(theVar) =>
        env.get(theVar) match {
          case Some((typ, _)) => typ
          case None => throw TypeErrorException("Variable not in scope: " + theVar)
        }

      case BinaryOperationExpression(left, op, right) =>
        val leftType = typeof(left, env, funcs)
        val rightType = typeof(right, env, funcs)
        (leftType, op, rightType) match {
          case (IntType, LessThanOperator, IntType) => BoolType
          case (BoolType, AndOperator, BoolType) => BoolType
          case (IntType, AddOperator, IntType) => IntType
          case _ => throw TypeErrorException("Bad types: " + (leftType, op, rightType))
        }

      case ApplyExpression(name, params) =>
        val actualParamTypes = params.expressions.map(p => typeof(p, env, funcs))
        funcs.get(name) match {
          case Some((expectedParamTypes, returnType)) =>
            if (actualParamTypes != expectedParamTypes) {
              throw TypeErrorException("Call had incorrect params")
            }
            returnType
          case None =>
            throw TypeErrorException("No such function: " + name)
        }
    }

    exp.typ = expType
    expType
  }
}

case class TypeErrorException(msg: String) extends Exception(msg)
