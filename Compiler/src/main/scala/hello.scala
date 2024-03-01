import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
sealed trait Token

// Symbol tokens
case object leftBracesToken extends Token
case object leftParenToken extends Token
case object rightParenToken extends Token
case object rightBracesToken extends Token
case object plusEqualsToken extends Token
case object minusEqualsToken extends Token
case object plusPlusToken extends Token
case object minusMinusToken extends Token
case object semicolonToken extends Token
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
case object ifToken extends Token
case object whileToken extends Token
case object decToken extends Token
case object assignToken extends Token
case object applyToken extends Token
case object sendtoken extends Token
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
case object cppToken extends Token
case object trueToken extends Token
case object falseToken extends Token
case object monitorToken extends Token
case object returnToken extends Token
case object waitToken extends Token


case class IdentifierToken(name: String) extends Token
case class IntegerLiteralToken(value: Int) extends Token
case class StringLiteralToken(value: String) extends Token

object Tokenizer{
  /*@main def hello(): Unit = {
    print("Hello World")
  }*/


  val leftBracesID: Regex = "\\{".r

  // basically just copied the example Dewey gave us
  // more developed version below, just putting this here for reference
  def prettyString(token: Token): String = {
    val id = IdentifierToken("foo")
    println(id.name)

    token match {
      case leftParenToken => "("
      case rightParenToken => ")"
      case IdentifierToken(name) => name
      case IntegerLiteralToken(value) => value.toString
    }
  }


  /*
  we could instead of splitting the input string, we could
  work through it as a list of characters, processing one character at a time.
  This would allow us to handle multi-character tokens more easily.
  The current setup probably won't work well for comments and string literals because we're stripping whitespace.
 */
  def test1tokenize(input:String): List[Token] = {
    // for yield is just shorthand for a map and a flatten
    val tokens = for{
      // we need to account for string literals and comments
      token <- input.split("\\s+")
    }yield token match{
      case "{" => leftBracesToken
      case "}" => rightBracesToken
      case "(" => leftParenToken
      case ")" => rightParenToken
      case "+=" => plusEqualsToken
      case "-=" => minusEqualsToken
      case "++" => plusPlusToken
      case "--" => minusMinusToken
      case "event" => eventToken
      case "actor" => actorToken
      case "statemachine" => statemachineToken
      case "initial" => initialToken
      case "state" => stateToken
      case "INTEGER" => intToken
      case "BOOL" => boolToken
      case ";" => semicolonToken

      case _ =>
        if(token.matches("[a-zA-Z][a-zA-Z0-9]*")){
          IdentifierToken(token)
        }else if(token.matches("[0-9]+")) {
          IntegerLiteralToken(token.toInt)
        }else if(token.matches("//.*") || token.matches("/\\*.*\\*/")){
          // ignore comments
          None
        // string literals
        }else if(token.matches("\"[a-zA-Z0-9]*\"")){
          StringLiteralToken(token)
        }else{
          throw new IllegalArgumentException(s"Unrecognized token: $token")
        }
    }
    tokens.toList
  }


  //********************************************

  // Creating a reserved words map
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
    ReserveWords += ("dec", decToken)
    ReserveWords += ("assign", assignToken)
    ReserveWords += ("apply", applyToken)
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
    ReserveWords += ("cpp", cppToken)
    ReserveWords += ("true", trueToken)
    ReserveWords += ("false", falseToken)
    ReserveWords += ("monitor", monitorToken)
    ReserveWords += ("return", returnToken)
    ReserveWords += ("wait", waitToken)




} // END OF MAIN

