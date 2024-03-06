import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
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

  val leftBracesID: Regex = "\\{".r

  // This is not done
          // ToDo: Implement Reserved words
          // ToDo: Double Check Symbols and Operators with grammar - DONE
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


  private def lexer(input:String): List[Token] = {
    val validTokens = List(
      "{" -> leftBracesToken,
      "}" -> rightBracesToken,
      "(" -> leftParenToken,
      ")" -> rightParenToken,
      "=" -> singleEqualsToken,
      "+=" -> plusEqualsToken,
      "-=" -> minusEqualsToken,
      ";" -> semicolonToken,
      "," -> commaToken,
      "*" -> multiplyToken,
      "/" -> divideToken,
      "%" -> moduloToken,
      "+" -> plusToken,
      "-" -> minusToken,
      "<<" -> doubleLeftArrowToken,
      ">>" -> doubleRightArrowToken,
      "<" -> leftArrowToken,
      ">" -> rightArrowToken,
      "<=" -> leftArrowEqualsToken,
      ">=" -> rightArrowEqualsToken,
      "==" -> doubleEqualsToken,
      "="  -> singleEqualsToken,
      "!=" -> notEqualsToken,
      "!" -> notToken,
      "&&" -> logicalAndToken,
      ")" -> rightParenToken,
      "(" -> leftParenToken,
      "}" -> rightBracesToken,
      "{" -> leftBracesToken,
      "||" -> logicalOrToken,
      "*=" -> mulEqualsToken,
      "/=" -> divEqualsToken,
      "%=" -> modEqualsToken,
      "<<=" -> doubleLeftArrowEqualsToken,
      ">>=" -> doubleRightArrowEqualsToken,
      "^=" -> upArrowEqualsToken,
      "^" -> upArrowToken
    )

    var ReserveWords = scala.collection.mutable.Map[String, Token]()
    ReserveWords += ("actor", actorToken)
    ReserveWords += ("on", onToken)
    ReserveWords += ("statemachine", statemachineToken)
    ReserveWords += ("state", stateToken)
    ReserveWords += ("entry", entryToken)
    ReserveWords += ("exit", exitToken)
    ReserveWords += ("func", funcToken)
    ReserveWords += ("initial", initialToken)
    ReserveWords += ("event", eventToken)
    ReserveWords += ("if", ifToken)
    ReserveWords += ("while", whileToken)
    ReserveWords += ("send", sendToken)
    ReserveWords += ("print", printToken)
    ReserveWords += ("println", printlnToken)
    ReserveWords += ("int", intToken)
    ReserveWords += ("string", stringToken)
    ReserveWords += ("bool", boolToken)
    ReserveWords += ("actorname", actornameToken)
    ReserveWords += ("statename", statenameToken)
    ReserveWords += ("eventname", eventnameToken)
    ReserveWords += ("go", goToken)
    ReserveWords += ("goif", goifToken)
    ReserveWords += ("else", elseToken)
    ReserveWords += ("const", constToken)
    ReserveWords += ("true", trueToken)
    ReserveWords += ("false", falseToken)
    ReserveWords += ("monitor", monitorToken)
    ReserveWords += ("return", returnToken)
    ReserveWords += ("wait", waitToken)

    def tokenize(chars:List[Char], currentToken: String, tokens: List[Token]):List[Token] = chars match {
      case Nil if currentToken.nonEmpty => tokens :+ tokenFromCurrent(currentToken)
      case Nil => tokens
      case head :: tail =>
        val newToken = currentToken + head
        // Add condition here to check for reserved words
        if(validTokens.exists(_._1 == newToken)) {
          tokenize(tail, "", tokens :+ validTokens.find(_._1 == newToken).get._2)
        } else if (currentToken.nonEmpty && head==' ' ) {
          tokenize(tail, "", tokens :+ tokenFromCurrent(currentToken))
        } else if (head==' ') {
          tokenize(tail, "", tokens)
        } else{
          tokenize(tail, newToken, tokens)
        }
    }

    def tokenFromCurrent(current: String): Token = current match {
      case "" => null //Ignore empty tokens
      case s if s.matches("[a-zA-Z][a-zA-Z0-9]*") => IdentifierToken(s)
      case s if s.matches("[0-9]+") => IntegerLiteralToken(s.toInt)
      case s if s.matches("//.*") || s.matches("/\\*.*\\*/") => null //This ignores comments
      case s if s.matches("\"[a-zA-Z0-9]*\"") => StringLiteralToken(s) // Needs to start with " / needs to also account for backslash
      case _ => throw new IllegalArgumentException(s"Unrecognized token: $current")
    }

    tokenize(input.toList, "", List.empty).filter(_ != null) // Filters out null
  }

} // End of object Tokenizer

