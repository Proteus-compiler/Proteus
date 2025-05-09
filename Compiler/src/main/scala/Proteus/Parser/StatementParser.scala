package Proteus.Parser

import Proteus.AbstractSyntaxTree.{
  Statement,
  Block,
  IfStmt,
  WhileStmt,
  DecStmt,
  AssignStmt,
  ExitStmt,
  ReturnStmt,
  VarName,
  ParenExpression
}

import Proteus.Tokenizer.{
  Token,
  ifToken,
  elseToken,
  whileToken,
  returnToken,
  exitToken,
  assignToken,
  semicolonToken,
  leftBracesToken,
  rightBracesToken,
  leftParenToken,
  rightParenToken,
  IntegerLiteralToken
}

import Proteus.Parser.ParserCombinators.{
  Parser,
  map,
  flatMap,
  or,
  many,
  succeed
}

import Proteus.Parser.TokenParsers.{
  token,
  varName,
  parseType
}

import Proteus.Parser.ExpressionParser.{
  expression,
  parenExpr
}

object StatementParser {

  // Parses a block: { stmt* }
  val block: Parser[Block] = tokens => tokens match {
    case `leftBracesToken` :: tail =>
      many(statement)(tail).flatMap {
        case (stmts, rest1) =>
          rest1 match {
            case `rightBracesToken` :: rest2 =>
              Some((Block(stmts), rest2))
            case _ => None
          }
      }
    case _ => None
  }

  // Parses a declaration: type var = expr ;
  val decStmt: Parser[Statement] =
    flatMap(parseType) { typ =>
      flatMap(varName) { name =>
        flatMap(token(assignToken)) { _ =>
          flatMap(expression) { expr =>
            map(token(semicolonToken)) { _ =>
              DecStmt(typ, name, expr)
            }
          }
        }
      }
    }

  // Parses an assignment: var = expr ;
  val assignStmt: Parser[Statement] =
    flatMap(varName) { name =>
      flatMap(token(assignToken)) { _ =>
        flatMap(expression) { expr =>
          map(token(semicolonToken)) { _ =>
            AssignStmt(name, expr)
          }
        }
      }
    }

  // Parses a return statement: return expr ;
  val returnStmt: Parser[Statement] =
    flatMap(token(returnToken)) { _ =>
      flatMap(expression) { expr =>
        map(token(semicolonToken)) { _ =>
          ReturnStmt(expr)
        }
      }
    }

  // Parses exit(NUMBER);
  val exitStmt: Parser[Statement] = tokens => tokens match {
    case `exitToken` :: `leftParenToken` :: IntegerLiteralToken(num) :: `rightParenToken` :: `semicolonToken` :: tail =>
      Some((ExitStmt(num), tail))
    case _ => None
  }

  // Parses a while loop: while (expr) block
  val whileStmt: Parser[Statement] =
    flatMap(token(whileToken)) { _ =>
      flatMap(parenExpr) {
        case ParenExpression(guard) =>
            map(block) { body =>
                WhileStmt(guard, body)
            } 
        }
    }

  // Parses an if statement: if (expr) block [else block]
  val ifStmt: Parser[Statement] =
    flatMap(token(ifToken)) { _ =>
      flatMap(parenExpr) {
        case ParenExpression(guard) =>
            flatMap(block) { ifTrue =>
            flatMap(
                or(
                flatMap(token(elseToken)) { _ =>
                    map(block)(Some(_))
                },
                succeed(None)
                )
            ) { maybeElse =>
                map(succeed(()))(_ => IfStmt(guard, ifTrue, maybeElse))
            }
            }
        }
    }

  // Any single statement
  val statement: Parser[Statement] =
    or(decStmt,
    or(assignStmt,
    or(returnStmt,
    or(exitStmt,
    or(whileStmt, ifStmt)))))
}
