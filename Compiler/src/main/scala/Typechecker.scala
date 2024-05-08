/**
 * @author Spencer Sullivan
 */

sealed trait ReturnAnalysis
case object NoReturn extends ReturnAnalysis
case object MaybeReturns extends ReturnAnalysis
case object DefinitelyReturns extends ReturnAnalysis

object Typechecker {

  // gonna have to change this because i don't think our AST works the same way
  // as Dewey when it comes to function definitions, or we just need function names in the AST
  def makeFunctionMap(prog: Program): Map[FuncName, (Seq[Type], Type)] = {
    //prog.funcs.foldLeft(Map[FunctionName, (Seq[Type], Type)]())((accum, cur) => {
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
    //prog.funcs.foreach(func => typecheckFunc(func, functionMapping))
    prog.defFuncs.foreach(func => typecheckFunc(func, functionMapping))
  }

  // REALLY not sure about this one
  def makeEnv(args: Seq[FormalFuncArgs], scopeLevel: Int): Map[VarName, (Type, Int)] = {
    args.foldLeft(Map[VarName, (Type, Int)]())((accum, cur) => {
      if (accum.contains(cur.theVar) {
        throw TypeErrorException("Duplicate variable name: " + cur.theVar)
      }
      accum + (cur.theVar -> (cur.Type, scopeLevel))
    })
  }

  // also really not sure about this one
  // funcsWithOverloading: Map[(FunctionName, Seq[Type]), Type]
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
      case _ => {
        throw TypeErrorException("Function might not return: " + func)
      }
    }
  }

  def typecheckStmts(
                      stmts: Seq[Statement],
                      scopeLevel: Int,
                      env: Map[VarName, (Type, Int)],
                      returnType: Type): (Map[VarName, (Type, Int)], ReturnAnalysis) = {
    stmts.foldLeft((env, NoReturn))((accum, curStmt) => {
      val (curEnv, curReturn) = accum
      val (nextEnv, stmtReturn) = typecheckStmts(curStmt, scopeLevel, curEnv, returnType)
      val nextReturn = (curReturn, stmtReturn) match {
        case (NoReturn, DefinitelyReturns) => DefinitelyReturns
        case (NoReturn, MaybeReturns) => MaybeReturns
        case (NoReturn, NoReturn) => NoReturn
        case (MaybeReturns, NoReturn) => MaybeReturns
        case (MaybeReturns, MaybeReturns) => MaybeReturns
        case (MaybeReturns, DefinitelyReturns) => DefinitelyReturns
        case (DefinitelyReturns, _) => throw TypeErrorException("Dead code: " + curStmt)
      }
      (nextEnv, nextReturn)
    })
  }

  def assertTypesSame(expected: Type, received: Type): Unit = {
    if (expected != received) {
      throw TypeErrorException(
        "Expected type: " + expected +
          "; received type: " + received)
    }
  }

  // TODO: finish first-order functions, go higher-order
  def typecheck(
                 stmt: Statement,
                 scopeLevel: Int,
                 env: Map[VarName, (Type, Int)],
                 funcs: Map[FuncName, (Seq[Type], Type)],
                 returnType: Type): (Map[VarName, (Type, Int)], ReturnAnalysis) = {
    stmt match {
      case ReturnStmt(exp) => {
        assertTypesSame(returnType, typeof(exp, env, funcs))
        (env, DefinitelyReturns)
      }
      case PrintlnStmt(exp) => {
        typeof(exp, env, funcs)
        (env, NoReturn)
      }
      // expectedType theVar = initializer;
      case DecStmt(expectedType, theVar, initializer) => {
        val receivedType = typeof(initializer, env, funcs)
        assertTypesSame(expectedType, receivedType)
        env.get(theVar) match {
          case Some((_, `scopeLevel`)) =>
            throw TypeErrorException("Name in same scope: " + theVar)
          case _ =>
            (env + (theVar -> (receivedType, scopeLevel)), NoReturn)
        }
      }
      case Block(stmts) => {
        val (_, returnAnalysis) =
          typecheckStmts(stmts, scopeLevel + 1, env, funcs, returnType)
        (env, returnAnalysis)
      }
    }
  }

  def typeof(
              exp: Expression,
              env: Map[VarName, (Type, Int)],
              funcs: Map[FuncName, (Seq[Type], Type)]): Type = {
    // if (exp instanceof IntegerLiteralExp) {
    //   return new IntType();
    // } else {
    val expType = exp match {
      case IntLiteralExpression(_) => IntType
      case BoolLiteralExpression(_) => BoolType
      case VarExpression(theVar) => {
        // is the variable in scope?
        // if so, what's the type of the variable?
        env.get(theVar) match {
          case Some((typ, _)) => typ
          case None => throw TypeErrorException("Variable not in scope: " + theVar)
        }
      }
      case BinaryOperationExpression(left, op, right) => {
        val leftType: Type = typeof(left, env, funcs)
        val rightType: Type = typeof(right, env, funcs)
        // if (leftType instanceof IntType &&
        //     op instanceof LessThanOp &&
        //     rightType instanceof IntType) {
        //    return new BoolType();
        // }
        val tup = (leftType, op, rightType)
        tup match {
          case (IntType, LessThanOperator, IntType) => BoolType
          // case (IntType, LessThanOp, BoolType) => {
          //   right.typeShouldBe = IntType
          case (BoolType, AndOperator, BoolType) => BoolType
          case (IntType, AddOperator, IntType) => IntType
          case _ => throw TypeErrorException("Bad types: " + tup)
        }
      } // BinopExp
      case ApplyExpression(name, params) => {
        //val actualParamTypes = params.map(param => typeof(param, env, funcs))
        val actualParamTypes = params.expressions.map(param => typeof(param, env, funcs))
        funcs.get(name) match {
          case Some((expectedParamTypes, returnType)) => {
            if (actualParamTypes != expectedParamTypes) {
              throw TypeErrorException("Call had incorrect params")
            }
            returnType
          }
          case None => throw TypeErrorException("No such function: " + name)
        }
      } // CallExp
    }
    exp.typ = expType
    expType
  }
}

case class TypeErrorException(msg: String) extends Exception(msg)