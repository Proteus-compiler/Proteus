/** @author Morgan Barrett */

//import AST
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator._

/*
Sequence (Seq[]) is used to represent indexed sequences that have a defined order of element i.e. guaranteed immutable. 
The elements of sequences can be accessed using their indexes. 
Sequences can also be accessed reversibly using the method reverse and reverseIterator
*/
case class TokenRead(tokens: Seq[Token]) extends Reader[Token]{

}

// Parser combinator that checks if the first token in the list matches the expected token. Consumes it if it does.
// Returns none if the next token does not match the expected token 
// => operator takes one type and returns another
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