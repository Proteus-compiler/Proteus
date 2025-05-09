package Proteus.Parser

import Proteus.AbstractSyntaxTree.{
  Expression,
  IntLiteralExpression,
  StringLiteralExpression,
  BoolLiteralExpression,
  VarExpression,
  VarName,
  FuncName,
  ApplyExpression,
  ExprListParen,
  ParenExpression,
  BinaryOperator,
  AddOperator,
  SubtractOperator,
  MultiplyOperator,
  DivideOperator,
  ModuloOperator,
  BinaryOperationExpression
}

import Proteus.Tokenizer.{
  Token,
  Tokenizer,
  IdentifierToken,
  IntegerLiteralToken,
  StringLiteralToken,
  trueToken,
  falseToken,
  leftParenToken,
  rightParenToken,
  commaToken,
  addToken,
  subtractToken,
  multiplyToken,
  divideToken,
  moduloToken
}

import Proteus.Parser.ParserCombinators.{
  Parser,
  map,
  or,
  flatMap,
  sepBy
}

import Proteus.Parser.TokenParsers.{
  token,
  intLiteralExpr,
  stringLiteralExpr,
  boolLiteralExpr,
  identifier,
  varName
}

object ExpressionParser {

  val variableExpr: Parser[VarExpression] =
    map(varName)(VarExpression.apply)

  val valueExpr: Parser[Expression] =
    or(intLiteralExpr,
    or(stringLiteralExpr,
    or(boolLiteralExpr, variableExpr)))

  val applyExpr: Parser[ApplyExpression] = tokens => tokens match {
    case IdentifierToken(name) :: `leftParenToken` :: tail =>
      parseArgs(tail).flatMap {
        case (args, afterArgs) =>
          afterArgs match {
            case `rightParenToken` :: rest =>
              Some((ApplyExpression(FuncName(name), ExprListParen(args)), rest))
            case _ => None
          }
      }
    case _ => None
  }

  def parseArgs(tokens: List[Token]): Option[(Seq[Expression], List[Token])] =
    sepBy(expression, token(commaToken))(tokens)

  val parenExpr: Parser[ParenExpression] = tokens => tokens match {
    case `leftParenToken` :: rest =>
      expression(rest).flatMap {
        case (exprInside, afterExpr) =>
          afterExpr match {
            case `rightParenToken` :: remaining =>
              Some((ParenExpression(exprInside), remaining))
            case _ => None
          }
      }
    case _ => None
  }

  val binaryOp: Parser[BinaryOperator] = {
    case `addToken` :: tail => Some((AddOperator, tail))
    case `subtractToken` :: tail => Some((SubtractOperator, tail))
    case `multiplyToken` :: tail => Some((MultiplyOperator, tail))
    case `divideToken` :: tail => Some((DivideOperator, tail))
    case `moduloToken` :: tail => Some((ModuloOperator, tail))
    case _ => None
  }

  val binaryExpr: Parser[Expression] = tokens => {
    simpleExpr(tokens).flatMap {
      case (left, rest1) =>
        binaryOp(rest1).flatMap {
          case (op, rest2) =>
            expression(rest2).map {
              case (right, rest3) =>
                (BinaryOperationExpression(left, op, right), rest3)
            }
        }
    }
  }

  val simpleExpr: Parser[Expression] =
    or(parenExpr,
    or(applyExpr,
    or(variableExpr,
    or(intLiteralExpr,
    or(stringLiteralExpr, boolLiteralExpr)))))

  val expression: Parser[Expression] = or(binaryExpr, simpleExpr)
}
