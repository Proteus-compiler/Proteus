import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
sealed trait Token

//case object extends Token
//case object extends Token



case object leftBracesToken extends Token
case object leftParenToken extends Token
case object rightParenToken extends Token
case object rightBracesToken extends Token
case object plusEqualsToken extends Token
case object minusEqualsToken extends Token
case object plusPlusToken extends Token
case object minusMinusToken extends Token
case object eventToken extends Token
case object actorToken extends Token
//case object boolToken extends Token
case object statemachineToken extends Token
case object initialToken extends Token
case object stateToken extends Token
case object INTEGERToken extends Token
case object BOOLToken extends Token
case object semicolonToken extends Token

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
      case "INTEGER" => INTEGERToken
      case "BOOL" => BOOLToken
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
  // List of reserved words
  val WordList: List[String] = List("word1", "word2", "word3")

  // "appending" to an immutable list by making a new list that includes previous list plus new items
  // I think this might be the way to go for making a list
  val WordListTwo = "word4" :: WordList

  // Alternatively, we can make a mutable list
  var Words = new ListBuffer[String]()
  Words += "word1"
  Words += "word2"


  // Converting a list to a map
  val ReserveMap1: Map[Int, String] = List(1 -> "words").toMap

  // Creating a map without a list
  val ReserveMap2: Map[Int, String] = Map(100 -> "word", 101 -> "words")



} // END OF MAIN

