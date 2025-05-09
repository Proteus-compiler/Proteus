package Proteus.Parser

import Proteus.AbstractSyntaxTree.{
  Program,
  DefEvent,
  DefGlobalConst,
  DefFunc,
  DefActor,
  ActorItem,
  VarName,
  FuncName,
  Type,
  ConstExpr,
  Expression,
  FormalFuncArgs,
  EventName,
  Block,
  IntConstLiteralExpression,
  StringConstLiteralExpression,
  BoolConstLiteralExpression
}

import Proteus.Tokenizer.{
  Token,
  constToken,
  funcToken,
  eventToken,
  IdentifierToken,
  assignToken,
  semicolonToken,
  leftParenToken,
  rightParenToken,
  leftBracesToken,
  rightBracesToken,
  arrowToken,
  commaToken
}

import Proteus.Parser.ParserCombinators.{
  Parser,
  map,
  flatMap,
  or,
  many,
  sepBy,
  succeed
}

import Proteus.Parser.TokenParsers.{
  token,
  identifier,
  varName,
  parseType,
  intLiteral,
  stringLiteral,
  boolLiteral
}

import Proteus.Parser.ExpressionParser.{
  expression
}

import Proteus.Parser.StatementParser.{
  block
}

object TopLevelParser {

  // const TYPE NAME = CONST_EXPR ;
  val defGlobalConst: Parser[DefGlobalConst] =
    flatMap(token(constToken)) { _ =>
      flatMap(parseType) { typ =>
        flatMap(varName) { name =>
          flatMap(token(assignToken)) { _ =>
            flatMap(constExpr) { value =>
              map(token(semicolonToken)) { _ =>
                DefGlobalConst(typ, name, value)
              }
            }
          }
        }
      }
    }

  // event NAME { TYPE [, TYPE]* } ;
  val defEvent: Parser[DefEvent] =
    flatMap(token(eventToken)) { _ =>
      flatMap(identifier) { name =>
        flatMap(token(leftBracesToken)) { _ =>
          flatMap(sepBy(parseType, token(commaToken))) { types =>
            flatMap(token(rightBracesToken)) { _ =>
              map(token(semicolonToken)) { _ =>
                DefEvent(EventName(name), types)
              }
            }
          }
        }
      }
    }

  // func NAME (FormalFuncArgs) [-> TYPE] Block
  val defFunc: Parser[DefFunc] =
    flatMap(token(funcToken)) { _ =>
      flatMap(identifier) { name =>
        flatMap(formalFuncArgs) { args =>
          flatMap(optionalReturnType) { maybeRet =>
            map(block) { body =>
              DefFunc(name, args, maybeRet, body)
            }
          }
        }
      }
    }

  // '(' Type VarName ')'
  val formalFuncArgs: Parser[FormalFuncArgs] = tokens => tokens match {
    case `leftParenToken` :: rest =>
      parseType(rest).flatMap {
        case (typ, IdentifierToken(name) :: `rightParenToken` :: tail) =>
          Some((FormalFuncArgs(typ, VarName(name)), tail))
        case _ => None
      }
    case _ => None
  }

  // '->' Type | nothing
  val optionalReturnType: Parser[Option[Type]] =
    or(
      flatMap(token(arrowToken)) { _ =>
        map(parseType)(Some(_))
      },
      succeed(None)
    )

  // full program
  val program: Parser[Program] =
    flatMap(many(defEvent)) { events =>
      flatMap(many(defGlobalConst)) { consts =>
        flatMap(many(defFunc)) { funcs =>
          flatMap(many(ActorParser.defActor)) { actors =>
            map(succeed(()))(_ => Program(events, consts, funcs, actors))
          }
        }
      }
    }

  def parseFromTokens(tokens: List[Token]): Option[Program] = {
    program(tokens).collect {
      case (prog, Nil) => prog
    }
  }

  // constExpr used in const and member declarations
  def constExpr: Parser[ConstExpr] = {
    or(
      map(intLiteral)(IntConstLiteralExpression.apply),
      or(
        map(stringLiteral)(StringConstLiteralExpression.apply),
        map(boolLiteral)(BoolConstLiteralExpression.apply)
      )
    )
  }
}
