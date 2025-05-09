package Proteus.Parser

import Proteus.AbstractSyntaxTree.{
  ActorItem,
  ActorName,
  Block,
  DefActor,
  DefActorOn,
  DefEntry,
  DefExit,
  DefHSM,
  DefMember_ActorItem,
  DefMethod_ActorItem,
  DefOn,
  DefState,
  ElseGoStmt,
  EventMatch,
  GoIfStmt,
  GoStmt,
  InitialState,
  JustGoStmt,
  OnBlock,
  OnBody,
  ParenExpression,
  StateItem,
  StateName,
  Type,
  VarName
}

import Proteus.Tokenizer.{
  Token,
  actorToken,
  assignToken,
  commaToken,
  entryToken,
  exitToken,
  funcToken,
  goToken,
  goifToken,
  IdentifierToken,
  initialToken,
  leftBracesToken,
  onToken,
  rightBracesToken,
  semicolonToken,
  stateToken,
  statemachineToken,
  elseToken
}

import Proteus.Parser.ExpressionParser.{
  parenExpr
}

import Proteus.Parser.ParserCombinators.{
  Parser,
  flatMap,
  many,
  map,
  or,
  sepBy,
  succeed
}

import Proteus.Parser.StatementParser.block
import Proteus.Parser.TokenParsers.{
  identifier,
  parseType,
  token,
  varName
}
import Proteus.Parser.TopLevelParser.{
  constExpr,
  formalFuncArgs,
  optionalReturnType
}

object ActorParser {

  // ────── Shared Parsers ──────

  val stateItem: Parser[StateItem] =
    or(defOn,
    or(initialState,
    or(defEntry,
    or(defExit,
       defState))))

  val actorItem: Parser[ActorItem] =
    or(defMethod,
    or(defMember,
    or(defHSM,
       defActorOn)))

  val goStmt: Parser[GoStmt] = or(goIfStmt, justGoStmt)

  val onBody: Parser[OnBody] =
    or(map(block)(OnBlock.apply), goStmt)

  // ────── Top-Level Actor Definition ──────

  val defActor: Parser[DefActor] =
    flatMap(token(actorToken)) { _ =>
      flatMap(identifier) { name =>
        flatMap(token(leftBracesToken)) { _ =>
          flatMap(many(actorItem)) { items =>
            map(token(rightBracesToken)) { _ =>
              DefActor(ActorName(name), items)
            }
          }
        }
      }
    }

  // ────── Actor Items ──────

  val defMember: Parser[ActorItem] =
    flatMap(parseType) { typ =>
      flatMap(varName) { name =>
        flatMap(token(assignToken)) { _ =>
          flatMap(constExpr) { value =>
            map(token(semicolonToken)) { _ =>
              DefMember_ActorItem(typ, name, value)
            }
          }
        }
      }
    }

  val defMethod: Parser[ActorItem] =
    flatMap(token(funcToken)) { _ =>
      flatMap(identifier) { name =>
        flatMap(formalFuncArgs) { args =>
          flatMap(optionalReturnType) { maybeRet =>
            map(block) { body =>
              DefMethod_ActorItem(name, Seq((args.typ, args.theVar.name)), maybeRet, body)
            }
          }
        }
      }
    }

  val defHSM: Parser[ActorItem] =
    flatMap(token(statemachineToken)) { _ =>
      flatMap(token(leftBracesToken)) { _ =>
        flatMap(many(stateItem)) { items =>
          map(token(rightBracesToken)) { _ =>
            DefHSM(items)
          }
        }
      }
    }

  val defActorOn: Parser[ActorItem] =
    flatMap(token(onToken)) { _ =>
      flatMap(eventMatch) { em =>
        flatMap(onBody) {
          case ob: OnBlock => succeed(DefActorOn(em, ob))
          case _ => _ => None // reject non-block for now
        }
      }
    }

  // ────── State Items ──────

  val initialState: Parser[StateItem] =
    flatMap(token(initialToken)) { _ =>
      flatMap(identifier) { name =>
        map(token(semicolonToken)) { _ =>
          InitialState(StateName(name))
        }
      }
    }

  val defEntry: Parser[StateItem] =
    flatMap(token(entryToken)) { _ =>
      map(block)(DefEntry.apply)
    }

  val defExit: Parser[StateItem] =
    flatMap(token(exitToken)) { _ =>
      map(block)(DefExit.apply)
    }

  val defState: Parser[StateItem] =
    flatMap(token(stateToken)) { _ =>
      flatMap(identifier) { name =>
        flatMap(token(leftBracesToken)) { _ =>
          flatMap(many(stateItem)) { items =>
            map(token(rightBracesToken)) { _ =>
              DefState(StateName(name), items)
            }
          }
        }
      }
    }

  val defOn: Parser[StateItem] =
    flatMap(token(onToken)) { _ =>
      flatMap(eventMatch) { em =>
        map(onBody) { ob =>
          DefOn(em, ob)
        }
      }
    }

  // ────── OnBody / GoStmt ──────

  val justGoStmt: Parser[GoStmt] =
    flatMap(token(goToken)) { _ =>
      flatMap(identifier) { name =>
        map(block) { b =>
          JustGoStmt(StateName(name), b)
        }
      }
    }

  val elseGoStmt: Parser[ElseGoStmt] =
    flatMap(token(goToken)) { _ =>
      flatMap(identifier) { name =>
        map(block) { b =>
          ElseGoStmt(StateName(name), b)
        }
      }
    }

  val goIfStmt: Parser[GoIfStmt] =
  flatMap(token(goifToken)) { _ =>
    flatMap(parenExpr) {
      case ParenExpression(cond) =>
        flatMap(identifier) { name =>
          flatMap(block) { thenBlock =>
            val elseBranchParser: Parser[Option[GoStmt]] =
              or(
                flatMap(token(elseToken)) { _ =>
                  or(
                    map(goIfStmt)(Some(_)),
                    map(elseGoStmt)(Some(_))
                  )
                },
                succeed(None)
              )

            map(elseBranchParser) { elseBranch =>
              GoIfStmt(cond, StateName(name), thenBlock, elseBranch)
            }
          }
        }
    }
  }


  // ────── Event Matching ──────

  val eventMatch: Parser[EventMatch] =
    flatMap(identifier) { eventName =>
      flatMap(token(leftBracesToken)) { _ =>
        flatMap(sepBy(varName, token(commaToken))) { vars =>
          map(token(rightBracesToken)) { _ =>
            EventMatch(Proteus.AbstractSyntaxTree.EventName(eventName), vars)
          }
        }
      }
    }
}
