import scala.annotation.tailrec

sealed trait Token

// Symbol tokens
case object leftBracesToken extends Token
case object rightBracesToken extends Token
case object leftParenToken extends Token
case object rightParenToken extends Token
case object assignToken extends Token
case object semicolonToken extends Token
case object commaToken extends Token
case object notToken extends Token
case object arrowToken extends Token // '->'

// Binary operator tokens
case object multiplyToken extends Token       // *
case object divideToken extends Token         // /
case object moduloToken extends Token         // %
case object addToken extends Token            // +
case object subtractToken extends Token       // -
case object leftShiftToken extends Token      // <<
case object rightShiftToken extends Token     // >>
case object lessThanToken extends Token       // <
case object greaterThanToken extends Token    // >
case object lessThanOrEqualToToken extends Token    // <=
case object greaterThanOrEqualToToken extends Token // >=
case object equalToToken extends Token        // ==
case object notEqualtoToken extends Token     // !=
case object exclusiveOrToken extends Token    // ^
case object andToken extends Token            // &&
case object orToken extends Token             // ||
case object multiplyAssignmentToken extends Token   // *=
case object divideAssignmentToken extends Token     // /=
case object moduloAssignmentToken extends Token     // %=
case object addAssignmentToken extends Token        // +=
case object subtractAssignmentToken extends Token   // -=
case object leftShiftAssignmentToken extends Token  // <<=
case object rightShiftAssignmentToken extends Token // >>=
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

object Tokenizer {

  def main(args: Array[String]): Unit = {
    val fileString = args.mkString(" ")
    val tokens = lexer(fileString)
    print(tokens)
  }

  def lexer(input: String): List[Token] = {
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
      val currentInput = skipWhitespaceAndComments(input)
      if (currentInput.isEmpty) accum
      else {
        val (token, tail) = readToken(currentInput)
        tokenize(tail, accum :+ token)
      }
    }

    def readToken(input: List[Char]): (Token, List[Char]) = {
      val cleaned = skipWhitespaceAndComments(input)
      tokenizeSymbol(cleaned)
        .orElse(tokenizeInteger(cleaned))
        .orElse(tokenizeStringLiteral(cleaned))
        .orElse(tokenizeWord(cleaned))
        .getOrElse(throw new IllegalArgumentException(s"Invalid token starting at: ${cleaned.take(10).mkString}"))
    }

    def tokenizeSymbol(input: List[Char]): Option[(Token, List[Char])] = input match {
      case '-' :: '>' :: tail => Some(arrowToken, tail)

      case '/' :: '=' :: tail => Some(divideAssignmentToken, tail)
      case '/' :: tail => Some(divideToken, tail)

      case '^' :: '=' :: tail => Some(exclusiveOrAssignmentToken, tail)
      case '^' :: tail => Some(exclusiveOrToken, tail)

      case '+' :: '=' :: tail => Some(addAssignmentToken, tail)
      case '+' :: tail => Some(addToken, tail)

      case '-' :: '=' :: tail => Some(subtractAssignmentToken, tail)
      case '-' :: tail => Some(subtractToken, tail)

      case '<' :: '<' :: '=' :: tail => Some(leftShiftAssignmentToken, tail)
      case '<' :: '<' :: tail => Some(leftShiftToken, tail)
      case '<' :: '=' :: tail => Some(lessThanOrEqualToToken, tail)
      case '<' :: tail => Some(lessThanToken, tail)

      case '>' :: '>' :: '=' :: tail => Some(rightShiftAssignmentToken, tail)
      case '>' :: '>' :: tail => Some(rightShiftToken, tail)
      case '>' :: '=' :: tail => Some(greaterThanOrEqualToToken, tail)
      case '>' :: tail => Some(greaterThanToken, tail)

      case '!' :: '=' :: tail => Some(notEqualtoToken, tail)
      case '!' :: tail => Some(notToken, tail)

      case '&' :: '&' :: tail => Some(andToken, tail)
      case '|' :: '|' :: tail => Some(orToken, tail)

      case '*' :: '=' :: tail => Some(multiplyAssignmentToken, tail)
      case '*' :: tail => Some(multiplyToken, tail)

      case '%' :: '=' :: tail => Some(moduloAssignmentToken, tail)
      case '%' :: tail => Some(moduloToken, tail)

      case '=' :: '=' :: tail => Some(equalToToken, tail)
      case '=' :: tail => Some(assignToken, tail)

      case ';' :: tail => Some(semicolonToken, tail)
      case ',' :: tail => Some(commaToken, tail)
      case '(' :: tail => Some(leftParenToken, tail)
      case ')' :: tail => Some(rightParenToken, tail)
      case '{' :: tail => Some(leftBracesToken, tail)
      case '}' :: tail => Some(rightBracesToken, tail)

      case _ => None
    }

    def tokenizeInteger(input: List[Char]): Option[(IntegerLiteralToken, List[Char])] = {
      val (num, tail) = takeWhileAndGetAfter(input)(_.isDigit)
      if (num.nonEmpty) {
        tail match {
          case symbl :: _ if symbl.isLetter => throw new IllegalArgumentException("Invalid Token: number followed by letter")
          case _ => Some(IntegerLiteralToken(num.mkString.toInt), tail)
        }
      } else None
    }

    def tokenizeStringLiteral(input: List[Char]): Option[(StringLiteralToken, List[Char])] = input match {
      case '"' :: tail =>
        val (content, rest) = takeWhileAndGetAfter(tail)(_ != '"')
        rest match {
          case '"' :: after => Some(StringLiteralToken(content.mkString), after)
          case _ => throw new IllegalArgumentException("Unterminated string literal")
        }
      case _ => None
    }

    def tokenizeWord(input: List[Char]): Option[(Token, List[Char])] = input match {
      case ch :: _ if ch.isLetter || ch == '_' =>
        val (wordChars, tail) = takeWhileAndGetAfter(input)(a => a.isLetterOrDigit || a == '_')
        val word = wordChars.mkString
        val token = ReservedWords.getOrElse(word, IdentifierToken(word))
        Some(token, tail)
      case _ => None
    }

    def takeWhileAndGetAfter[A](input: List[A])(check: A => Boolean): (List[A], List[A]) = {
      val before = input.takeWhile(check)
      val after = input.drop(before.size)
      (before, after)
    }

    @tailrec
    def skipWhitespaceAndComments(ch: List[Char]): List[Char] = ch match {
      case '\n' :: tail => skipWhitespaceAndComments(tail)
      case '/' :: '/' :: tail => skipWhitespaceAndComments(tail.dropWhile(_ != '\n'))
      case c :: tail if c.isWhitespace => skipWhitespaceAndComments(tail)
      case _ => ch
    }

    tokenize(input.toList, List.empty[Token])
  }

}
