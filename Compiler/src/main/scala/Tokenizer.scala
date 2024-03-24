import scala.annotation.tailrec

sealed trait Token

// Symbol tokens
case object leftBracesToken extends Token
case object rightBracesToken extends Token
case object leftParenToken extends Token
case object rightParenToken extends Token
case object singleEqualsToken extends Token
case object plusEqualsToken extends Token
case object minusEqualsToken extends Token
case object semicolonToken extends Token
case object commaToken extends Token
case object notToken extends Token
//BinOp tokens
case object multiplyToken extends Token // *
case object divideToken extends Token // /
case object moduloToken extends Token // %
case object plusToken extends Token // +
case object minusToken extends Token // -
case object doubleLeftArrowToken extends Token // <<
case object doubleRightArrowToken extends Token // >>
case object leftArrowToken extends Token // <
case object rightArrowToken extends Token // >
case object leftArrowEqualsToken extends Token // <=
case object rightArrowEqualsToken extends Token // >=
case object doubleEqualsToken extends Token // ==
case object notEqualsToken extends Token // !=
case object upArrowToken extends Token // ^
case object logicalAndToken extends Token // &&
case object logicalOrToken extends Token // ||
case object mulEqualsToken extends Token // *=
case object divEqualsToken extends Token // /=
case object modEqualsToken extends Token // %=
case object doubleLeftArrowEqualsToken extends Token // <<=
case object doubleRightArrowEqualsToken extends Token // >>=
case object upArrowEqualsToken extends Token // ^=
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

  // This is not done
          // ToDo: Implement Reserved words
          // ToDo: Double Check \" in string literal Regex
          // ToDo: Double Check String Literal, Identifier, Integer Literal regex.
  def main(args: Array[String]): Unit = {
    val tokens = lexer("event hello {}")
    print(tokens)
  }

  //from 430 github
   /* private Token tryTokenizeIdentifierOrReservedWord() {
        if (Character.isLetter(input.charAt(pos))) {
            final StringBuffer read = new StringBuffer();
            read.append(input.charAt(pos));
            pos++;
            while (pos < input.length() &&
                   Character.isLetterOrDigit(input.charAt(pos))) {
                read.append(input.charAt(pos));
                pos++;
            }
            final String asString = read.toString();
            Token reservedWord = RESERVED_WORDS.get(asString);
            if (reservedWord != null) {
                return reservedWord;
            } else {
                return new IdentifierToken(asString);
            }
        } else {
            return null;
        }
    }
   */

  def lexer(input:String): List[Token] = {

    /** ReservedWords
     *  @author Morgan Barrett
     *  Immutable Map that contains
     *  Key -> String
     *  Value -> Token
     * */
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

    @tailrec
    def tokenize(input: List[Char], accum: List[Token]): List[Token] = {
      if (input.isEmpty) accum
      else {
        val (token, tail) = readToken(input)
        tokenize(tail, accum :+ token)
      }
    }

    def readToken(input: List[Char]): (Token, List[Char]) = {
      val (_, inputWithoutLeadingSpaces) = takeWhileAndGetAfter(input)(_.isWhitespace)
      tokenizeSymbol(inputWithoutLeadingSpaces)
        .orElse(tokenizeInteger(inputWithoutLeadingSpaces))
        .orElse(tokenizeWord(inputWithoutLeadingSpaces))
        .getOrElse(throw new IllegalArgumentException("Invalid Token"))
    }

    def tokenizeSymbol(input: List[Char]): Option[(Token, List[Char])] = {
      input match {
        //TODO: Account for comments
        //case '/' :: '*' :: '*' ::   tail =>
        //case '/' :: '*' ::          tail =>
        //case '/' :: '/' ::          tail =>

        case '^' :: '=' ::          tail => Some(upArrowEqualsToken, tail) //'^='
        case '^' ::                 tail => Some(upArrowToken, tail) //'^'

        case '+' :: '=' ::          tail => Some(plusEqualsToken, tail) //'+='
        case '+' ::                 tail => Some(plusToken, tail) // '+'

        case '-' :: '=' ::          tail => Some(minusEqualsToken, tail) //'-='
        case '-' ::                 tail => Some(minusToken, tail) // '-'

        case '<' :: '<' :: '=' ::   tail => Some(doubleLeftArrowEqualsToken, tail) // '<<='
        case '<' :: '<' ::          tail => Some(doubleLeftArrowToken, tail) //'<<'
        case '<' :: '=' ::          tail => Some(leftArrowEqualsToken, tail) //'<='
        case '<' ::                 tail => Some(leftArrowToken, tail) //'<'

        case '>' :: '>' :: '=' ::   tail => Some(doubleRightArrowEqualsToken, tail) //'>>='
        case '>' :: '>' ::          tail => Some(doubleRightArrowToken, tail) //'>>'
        case '>' :: '=' ::          tail => Some(rightArrowEqualsToken, tail) //'>='
        case '>' ::                 tail => Some(rightArrowToken, tail) //'>'

        case '!' :: '=' ::          tail => Some(notEqualsToken, tail) //'!='
        case '!' ::                 tail => Some(notToken, tail) //'!'
        case '&' :: '&' ::          tail => Some(logicalAndToken, tail) //'&&'
        case '|' :: '|' ::          tail => Some(logicalOrToken, tail) //'||'

        case '*' :: '=' ::          tail => Some(mulEqualsToken, tail) //'*='
        case '*' ::                 tail => Some(multiplyToken, tail) //'*'

        case '/' :: '=' ::          tail => Some(divEqualsToken, tail) //'/='
        case '/' ::                 tail => Some(divideToken, tail) //'/'

        case '%' :: '=' ::          tail => Some(modEqualsToken, tail) //'%='
        case '%' ::                 tail => Some(moduloToken, tail) //'%'

        case '=' :: '=' ::          tail => Some(doubleEqualsToken, tail) //'=='
        case '=' ::                 tail => Some(singleEqualsToken, tail) //'='

        //TODO: Insert missing symbols here

        case ';' ::                 tail => Some(semicolonToken, tail) //';'
        case ',' :: tail => Some(commaToken, tail) //','
        case '(' :: tail => Some(leftParenToken, tail) //'('
        case ')' :: tail => Some(rightParenToken, tail) //')'
        case '{' :: tail => Some(leftBracesToken, tail) //'{'
        case '}' :: tail => Some(rightBracesToken, tail) //'}'
        case _ => None
      }
    }

    def tokenizeInteger(input: List[Char]): Option[(IntegerLiteralToken, List[Char])] = {
      val (num, tail) = takeWhileAndGetAfter(input)(_.isDigit)
      if (num.nonEmpty) {
        tail match {
          case symbl :: other if symbl.isLetter => throw IllegalArgumentException("Invalid Token")
          case _ => Some(IntegerLiteralToken(num.mkString.toInt), tail)
        }
      } else None
    }

    def tokenizeWord(input: List[Char]): Option[(Token, List[Char])] = {
      input match {
        case ch :: tail if ch.isLetter =>
          val (c, tail) = takeWhileAndGetAfter(input)(_.isLetterOrDigit)
          val word = c.mkString
          val token = ReservedWords.getOrElse(word, IdentifierToken(word))
          Some(token, tail)
        case _ => None
      }
    }

    def takeWhileAndGetAfter[A](input: List[A])(check: (A) => Boolean): (List[A], List[A]) = {
      val before = input.takeWhile(check)
      (before, input.drop(before.size))
    }

    tokenize(input.toList, List.empty[Token])
  }
} // End of object Tokenizer

