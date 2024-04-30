import scala.annotation.tailrec

sealed trait Token

// Symbol tokens
case object leftBracesToken extends Token
case object rightBracesToken extends Token
case object leftParenToken extends Token
case object rightParenToken extends Token
case object assignToken extends Token
case object addAssignmentToken extends Token
case object subtractAssignmentToken extends Token
case object semicolonToken extends Token
case object commaToken extends Token
case object notToken extends Token
//BinOp tokens
case object multiplyToken extends Token // *
case object divideToken extends Token // /
case object moduloToken extends Token // %
case object addToken extends Token // +
case object subtractToken extends Token // -
case object leftShiftAssignmentToken extends Token // <<
case object rightShiftAssignmentToken extends Token // >>
case object lessThanToken extends Token // <
case object greaterThanToken extends Token // >
case object lessThanOrEqualToToken extends Token // <=
case object greaterThanOrEqualToToken extends Token // >=
case object equalToToken extends Token // ==
case object notEqualtoToken extends Token // !=
case object exclusiveOrToken extends Token // ^
case object andToken extends Token // &&
case object orToken extends Token // ||
case object multiplyAssignmentToken extends Token // *=
case object divideAssignmentToken extends Token // /=
case object moduloAssignmentToken extends Token // %=
case object leftShiftToken extends Token // <<=
case object rightShiftToken extends Token // >>=
case object exclusiveOrAssignmentToken extends Token // ^=
// Reserved word tokens
case object actorToken extends Token
case object onToken extends Token
case object statemachineToken extends Token
case object stateToken extends Token
case object entryToken extends Token
case object exitToken extends Token
case object funcToken extends Token
case object initialToken extends Token
case object eventToken extends Token
case object ifToken extends Token
case object whileToken extends Token
case object sendToken extends Token
case object printToken extends Token
case object printlnToken extends Token
case object intToken extends Token
case object stringToken extends Token
case object boolToken extends Token
case object actornameToken extends Token
case object statenameToken extends Token
case object eventnameToken extends Token
case object goToken extends Token
case object goifToken extends Token
case object elseToken extends Token
case object constToken extends Token
case object trueToken extends Token
case object falseToken extends Token
case object monitorToken extends Token
case object returnToken extends Token
case object waitToken extends Token

case class IdentifierToken(name: String) extends Token
case class IntegerLiteralToken(value: Int) extends Token
case class StringLiteralToken(value: String) extends Token

object Tokenizer{

  //TODO: We need newLine Tokens? Maybe?
  /** main
   *  main function calls lexer function. Takes in an Array of Strings called args
   *  converts it into a string, and sends it to the lexer. args will be the input file.
   *
   * @param args an Array of Strings taken from the command line when main is called
   **/
  def main(args: Array[String]): Unit = {
    val fileString = args.mkString(" ")
    val tokens = lexer(fileString)
    print(tokens)
  }

  /** lexer
   * Takes in a string, and returns a List of tokens. The String is broken up into an array of characters,
   * and recursively checked if it matches a symbol, integer or identifier. If none, then an exception is thrown
   *
   * @author Jorge Enriquez
   * @author Morgan Barrett
   * @param input A String that holds the input file
   * @return a list of Tokens
   **/
  def lexer(input:String): List[Token] = {

    /** ReservedWords
     *  Immutable Map that contains
     *  Key -> String
     *  Value -> Token
     *  @author Morgan Barrett
     **/
    val ReservedWords: Map[String, Token] = Map(
      "actor" -> actorToken,
      "on" -> onToken,
      "statemachine" -> statemachineToken,
      "state" -> stateToken,
      "entry" -> entryToken,
      "exit" -> exitToken,
      "func" -> funcToken,
      "initial" -> initialToken,
      "event" -> eventToken,
      "if" -> ifToken,
      "while" -> whileToken,
      "send" -> sendToken,
      "print" -> printToken,
      "println" -> printlnToken,
      "int" -> intToken,
      "string" -> stringToken,
      "bool" -> boolToken,
      "actorname" -> actornameToken,
      "statename" -> statenameToken,
      "eventname" -> eventnameToken,
      "go" -> goToken,
      "goif" -> goifToken,
      "else" -> elseToken,
      "const" -> constToken,
      "true" -> trueToken,
      "false" -> falseToken,
      "monitor" -> monitorToken,
      "return" -> returnToken,
      "wait" -> waitToken
    )

    /** tokenize
     * Takes input, a List of Characters, and checks if the List is empty.
     *
     * If the List is NOT empty, the function readToken is called, sending the List of Characters (input)
     * as a parameter. A Token and a new List of characters is returned.
     * The Token is then added to the accum List of Tokens, and tokenize is recursively called.
     *
     * If the List is empty, accum, the List of Tokens, is returned.
     * @param input: The current List of Characters that are yet to be interpreted.
     * @param accum: A List of tokens that have been parsed from input
     * @return the List of tokens: accum
     * */
    @tailrec
    def tokenize(input: List[Char], accum: List[Token]): List[Token] = {
      val currentInput = skipWhitespaceAndComments(input)
      if (currentInput.isEmpty) accum
      else {
        val (token, tail) = readToken(currentInput)
        tokenize(tail, accum :+ token)
      }
    }

    /** readToken
     * After removing Leading whitespaces, readToken will check three functions: tokenizeSymbol, tokenizeInteger,
     * and tokenizeWord. If one of these functions returns a token, then readToken will return that token. If no token
     * is returned, readToken will throw an exception.
     * @param input: the current List of Characters waiting to be tokenized.
     * @return a new token and the remaining List of Characters
     **/
    def readToken(input: List[Char]): (Token, List[Char]) = {
      val inputWithoutLeadingSpaces = skipWhitespaceAndComments(input)
      tokenizeSymbol(inputWithoutLeadingSpaces)
        .orElse(tokenizeInteger(inputWithoutLeadingSpaces))
        .orElse(tokenizeWord(inputWithoutLeadingSpaces))
        .getOrElse(throw new IllegalArgumentException("Invalid Token"))
    }

    /** tokenizeSymbol
     * tokenizeSymbol uses pattern matching to determing what the leading character in input is. If a case
     * matches, it will check the following character incase of symbol with more than one character.
     *
     * Ex:
     * '/=' is two characters: '/' and '='. Although '/' is a token itself (divideToken) tokenizeSymbol will
     * check the next character for possible 2 character tokens. In this case, '=' follows, which returns
     * divideAssignmentToken ('/=') and not divideToken ('/')
     *
     * if no case is met, the function returns none.
     *
     * @param input: the current List of Characters waiting to be tokenized.
     * @return an optional pair of Token and the remaining List of Characters
     * */
    def tokenizeSymbol(input: List[Char]): Option[(Token, List[Char])] = {
      input match {
        case '/' :: '=' ::          tail => Some(divideAssignmentToken, tail) //'/='
        case '/' ::                 tail => Some(divideToken, tail) //'/'

        case '^' :: '=' ::          tail => Some(exclusiveOrAssignmentToken, tail) //'^='
        case '^' ::                 tail => Some(exclusiveOrToken, tail) //'^'

        case '+' :: '=' ::          tail => Some(addAssignmentToken, tail) //'+='
        case '+' ::                 tail => Some(addToken, tail) // '+'

        case '-' :: '=' ::          tail => Some(subtractAssignmentToken, tail) //'-='
        case '-' ::                 tail => Some(subtractToken, tail) // '-'

        case '<' :: '<' :: '=' ::   tail => Some(leftShiftAssignmentToken, tail) // '<<='
        case '<' :: '<' ::          tail => Some(leftShiftToken, tail) //'<<'
        case '<' :: '=' ::          tail => Some(lessThanOrEqualToToken, tail) //'<='
        case '<' ::                 tail => Some(lessThanToken, tail) //'<'

        case '>' :: '>' :: '=' ::   tail => Some(rightShiftAssignmentToken, tail) //'>>='
        case '>' :: '>' ::          tail => Some(rightShiftToken, tail) //'>>'
        case '>' :: '=' ::          tail => Some(greaterThanOrEqualToToken, tail) //'>='
        case '>' ::                 tail => Some(greaterThanToken, tail) //'>'

        case '!' :: '=' ::          tail => Some(notEqualtoToken, tail) //'!='
        case '!' ::                 tail => Some(notToken, tail) //'!'
        case '&' :: '&' ::          tail => Some(andToken, tail) //'&&'
        case '|' :: '|' ::          tail => Some(orToken, tail) //'||'

        case '*' :: '=' ::          tail => Some(multiplyAssignmentToken, tail) //'*='
        case '*' ::                 tail => Some(multiplyToken, tail) //'*'

        case '%' :: '=' ::          tail => Some(moduloAssignmentToken, tail) //'%='
        case '%' ::                 tail => Some(moduloToken, tail) //'%'

        case '=' :: '=' ::          tail => Some(equalToToken, tail) //'=='
        case '=' ::                 tail => Some(assignToken, tail) //'='

        //TODO: Insert missing symbols here (only if there are any)

        case ';' ::                 tail => Some(semicolonToken, tail) //';'
        case ',' ::                 tail => Some(commaToken, tail) //','
        case '(' ::                 tail => Some(leftParenToken, tail) //'('
        case ')' ::                 tail => Some(rightParenToken, tail) //')'
        case '{' ::                 tail => Some(leftBracesToken, tail) //'{'
        case '}' ::                 tail => Some(rightBracesToken, tail) //'}'
        case _                           => None
      }
    }

    /** tokenizeInteger
     * tokenizeInteger will split input into two lists, one with numbers (num), and the other with
     * remaining characters (tail). If num is empty, then there where no leading numbers in input and
     * None is returned. If num is non-empty, then tail is checked for any incorrect syntax. If incorrect
     * syntax is found, an exception is thrown. Otherwise, a IntergerLiteralToken and the remaining list
     * of Characters are returned.
     *
     * @param input: remaining List of Characters waiting to be interpreted.
     * @return an optional pair of Token and the remaining List of Characters
     * */
    def tokenizeInteger(input: List[Char]): Option[(IntegerLiteralToken, List[Char])] = {
      val (num, tail) = takeWhileAndGetAfter(input)(_.isDigit)
      if (num.nonEmpty) {
        tail match {
          case symbl :: other if symbl.isLetter => throw IllegalArgumentException("Invalid Token")
          case _ => Some(IntegerLiteralToken(num.mkString.toInt), tail)
        }
      } else None
    }

    /** tokenizeWord
     * tokenizeWord will take all characters that are not whitespace and turn them into an IdentifierToken.
     * Will return None if leading character in input is not a letter. Otherwise will begin building a word.
     * As the word is building, it is checked with ReservedWords. If ReservedWords holds that word, it returns
     * a token from ReservedWords. If not, it will return an IdentifierToken and the remaining list.
 *
     * @param input: remaining List of Characters waiting to be interpreted.
     * @return an optional pair of Token and the remaining List of Characters
     * */
    def tokenizeWord(input: List[Char]): Option[(Token, List[Char])] = {
      input match {
        case ch :: tail if ch.isLetter || ch == '_' =>
          val (c, tail) = takeWhileAndGetAfter(input)(a => a.isLetterOrDigit || a == '_')
          val word = c.mkString
          val token = ReservedWords.getOrElse(word, IdentifierToken(word))
          Some(token, tail)
        case _ => None
      }
    }

    /** takeWhileAndGetAfter
     * Uses the function sent to determine a split in input: Before and After. Returns both.
     * @param input remaining List of Characters waiting to be interpreted.
     * @param function A higher order function that returns a boolean value.
     * @return Two lists, separated by the boolean function.
     * */
    def takeWhileAndGetAfter[A](input: List[A])(check: (A) => Boolean): (List[A], List[A]) = {
      val before = input.takeWhile(check)
      val after = input.drop(before.size)
      (before, after)
    }

    /** skipWhitespaceAndComments
     * recursively takes ch and pattern matches to filter out any comments and whitespaces. The end
     * of comments are determined by a new line character '\n'
     * @param ch List of Characters waiting to be interpreted.
     * @return a filtered list with no leading whitespaces or comments
     * */
    @tailrec
    def skipWhitespaceAndComments(ch: List[Char]): List[Char] = {
      ch match {
        case '\n' ::       tail => skipWhitespaceAndComments(tail)
        case '/' :: '/' :: tail => skipWhitespaceAndComments(tail.dropWhile(_ != '\n'))
        case c :: tail if c.isWhitespace => skipWhitespaceAndComments(tail)
        case _ => ch
      }
    }

    //Initiallizes Tokenize. Creates Array of Characters and sends an empty List
    tokenize(input.toList, List.empty[Token])
  }
  
} // End of object Tokenizer

