
/** @author Morgan Barrett */

import AST
import Tokenizer
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.Parsers

//
//TODO: Error handling
//      Update Expressions in the grammar
//      
//

// Declaring a Parser type that takes in anything, cast as generic "A"
// Checks if the first token in the list matches the expected, returns none if no match
type Parser[A] = (List[Token]) => Option[(A, List[Token])]

// def function accepts a parameter "expected" of type Token, returns value of type Parser[Unit]
def token(expected: Token): Parser[Unit] = {
  (tokens: List[Token]) => {
     tokens match {
       case head :: tail if head == expected => Some(((), tail))
       case _ => None
    }
  }
}

// Function that parses Int tokens 
val integer: Parser[Int] = {
  (tokens: Token[Token]) => {
    tokens match {
      case IntegerToken(i) :: tail => Some((i, tail))
      case _ => None
    }
  }
}
/* 
  The 'and' function combines two parsers p1, p2 such that both must successfully parse their parts of the input in sequence.
  Parser p1 parses type A, p2 parses type B, returns parser tuple. Parser p1 is applied to an initial list of tokens (tokens1), 
  result is matched against remaining tokens (tokens2), if any. If successful, parser p2 is applied to list tokens2 of type a.
  If successful, it takes a tuple of type b and list of remaining tokens (tokens3), returns tuple of type a&b 
  along with remaining tokens. 'case _' covers any case where p1 or p2 fails
  */
def and[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = {
  (tokens1: List[Token]) => {
    p1(tokens1) match {
      case Some((a, tokens2)) => {
        p2(tokens2) match {
          case Some((b, tokens3)) => Some(((a, b), tokens3))
          case _ => None
        }
      }
      case _ => None
    }
  }
}
   
/* 
  The 'or' function combines two parsers p1 & p2 such that the combined parser succeeds if either p1 or p2
  successfully parses the input. It first tries to parse using p1, and if p1 fails, it then tries p2 with the same initial tokens.
*/
def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = {
  (tokens1: List[Token]) => {
    p1(tokens1) match {
      case Some((a, rest)) => Some((a, rest))
      case _ => {
        p2(tokens1) match {
          case Some((b, rest)) => Some((b, rest))
          case None => None
        }
      }
    }
  }
}
/*
  Parser combinator that parses input tokens of type A, the function 'f' converts them to type B. 
  Then pattern matches lists of tokens, as above. 
*/  
  def map[A, B](p: Parser[A], f: A => B): Parser[B] = {
    (tokens1: List[Token]) => {
      p(tokens1) match {
        case Some((a, tokens2)) => Some((f(a), tokens2))
        case _ => None
      }
    }
  }

/*
  Example method (from a different grammar) that parses in Expressions 
  Parser[(Unit, (Unit, (Variable, (Exp, (Exp, Unit)))))] 
  ^^ is used to map the result of the parser to a different type
*/
lazy val exp: Parser[Expression] =
  integer ^^ (i => IntegerLiteralExpresssion(i)) |
  variable ^^ (name => VariableExpression(name)) |
  (token(LeftParenToken) ~ token(LetToken) ~ variable ~ exp ~ exp ~ token(RightParenToken)) ^^ {
    case _ ~ _ ~ x ~ e1 ~ e2 ~ _ => LetExp(x, e1, e2)
  } |
  (token(LeftParenToken) ~ op ~ exp ~ exp ~ token(RightParenToken)) ^^ {
    case _ ~ o ~ e1 ~ e2 ~ _ => OpExp(o, e1, e2)
  }

  // ^^ is mapping
  // ^^^ is used for true/false (only one or the other are possible)
  // there is something in the scala library that is specifically used for int literals
  // lazy val exp allows you to use exp inside the method. if not lazy, it will be rejected
  // DefGlobalConst: 'const' Type VarName '=' ConstExpr ';'
  // 
  // def repsep[T] for rules with a *

/*
  BinOp Parser
*/
lazy val binop: Parser[BinaryOperator] =
  multiplyToken ^^ ((a: Unit) => MultiplyOperator) |
  divideToken ^^ ((a: Unit) => DivideOperator) |
  moduloToken ^^ ((a: Unit) => ModuloOperator) |
  addToken ^^ ((a: Unit) => AddOperator) |
  subtractToken ^^ ((a: Unit) => SubtractOperator) |
  leftShiftAssignmentToken ^^ ((a: Unit) => LeftShiftAssignmentOperator) |
  rightShiftAssignmentToken ^^ ((a: Unit) => RightShiftAssignmentOperator) |
  lessThanToken ^^ ((a: Unit) => LessThanOperator) |
  greaterThanToken ^^ ((a: Unit) => GreaterThanOperator) |
  lessThanOrEqualToToken ^^ ((a: Unit) => LessThanOrEqualToOperator) |
  greaterThanOrEqualToToken ^^ ((a: Unit) => GreaterThanOrEqualToOperator) |
  equalToToken ^^ ((a: Unit) => EqualToOperator) |
  notEqualToToken ^^ ((a: Unit) => NotEqualToOperator) |
  exclusiveOrToken ^^ ((a: Unit) => ExclusiveOrOperator) |
  andToken ^^ ((a: Unit) => AndOperator) |
  orToken ^^ ((a: Unit) => OrOperator) |
  multiplyAssignmentToken ^^ ((a: Unit) => MultiplyAssignmentOperator) |
  divideAssignmentToken ^^ ((a: Unit) => DivideAssignmentOperator) |
  moduloAssignmentToken ^^ ((a: Unit) => ModuloAssignmentOperator) |
  addAssignmentToken ^^ ((a: Unit) => AddAssignmentOperator) |
  subtractAssignmentToken ^^ ((a: Unit) => SubtractAssignmentOperator) |
  leftShiftToken ^^ ((a: Unit) => LeftShiftOperator) |
  rightShiftToken ^^ ((a: Unit) => RightShiftOperator)
  exclusiveOrAssignmentToken ^^ ((a: Unit) => ExclusiveOrAssignmentOperator)



// =================  Everything below is tail recursive, probably won't use =================
/*
  BinOp Parser EXAMPLE 2
*/
def binop: Parser[BinaryOperator] = tokens => tokens match {
  case Multiply :: tail => Some(Parser(MultiplyOperator, tail)) 
  case Divide :: tail => Some(Parser(DivideOperator, tail))
  case Modulo :: tail => Some(Parser(ModuloOperator, tail))
  case Add :: tail => Some(Parser(ModuloOperator, tail))
}

/*
  Type Parser
*/
def types: Parser[Type] = tokens => tokens match{
  case Int :: tail => Some(Parser(IntType, tail))
  case String :: tail => Some(Parser(StringType, tail))
  case Bool :: tail => Some(Parser(BoolType, tail))
}

def expression: Parser[Expression] = tokens => tokens match{
  case VarExp :: 
}
