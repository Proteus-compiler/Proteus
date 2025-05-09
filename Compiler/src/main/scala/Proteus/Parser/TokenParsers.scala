package Proteus.Parser

import Proteus.Tokenizer.{
  Token,
  IdentifierToken,
  IntegerLiteralToken,
  StringLiteralToken,
  trueToken,
  falseToken,
  intToken,
  stringToken,
  boolToken,
  actornameToken,
  statenameToken,
  eventnameToken
}

import Proteus.AbstractSyntaxTree.{
  VarName,
  Type,
  IntType,
  StringType,
  BoolType,
  ActorName,
  StateName,
  EventName,
  IntLiteralExpression,
  StringLiteralExpression,
  BoolLiteralExpression
}

import Proteus.Parser.ParserCombinators.{
  Parser,
  map
}

object TokenParsers {

  def token(expected: Token): Parser[Unit] = {
    case head :: tail if head == expected => Some(((), tail))
    case _ => None
  }

  val identifier: Parser[String] = {
    case IdentifierToken(name) :: tail => Some((name, tail))
    case _ => None
  }

  val varName: Parser[VarName] = map(identifier)(VarName.apply)

  val intLiteral: Parser[Int] = {
    case IntegerLiteralToken(value) :: tail => Some((value, tail))
    case _ => None
  }

  val intLiteralExpr: Parser[IntLiteralExpression] =
    map(intLiteral)(IntLiteralExpression.apply)

  val stringLiteral: Parser[String] = {
    case StringLiteralToken(value) :: tail => Some((value, tail))
    case _ => None
  }

  val stringLiteralExpr: Parser[StringLiteralExpression] =
    map(stringLiteral)(StringLiteralExpression.apply)

  val boolLiteral: Parser[Boolean] = {
    case `trueToken` :: tail  => Some((true, tail))
    case `falseToken` :: tail => Some((false, tail))
    case _ => None
  }

  val boolLiteralExpr: Parser[BoolLiteralExpression] =
    map(boolLiteral)(BoolLiteralExpression.apply)

  val parseType: Parser[Type] = {
    case `intToken` :: tail        => Some((IntType, tail))
    case `stringToken` :: tail     => Some((StringType, tail))
    case `boolToken` :: tail       => Some((BoolType, tail))
    case `actornameToken` :: tail  => Some((ActorName("actor")), tail)
    case `statenameToken` :: tail  => Some((StateName("state")), tail)
    case `eventnameToken` :: tail  => Some((EventName("event")), tail)
    case _ => None
  }
}
