import scala.util.matching.Regex
sealed trait Token

//case object extends Token
//case object extends Token



case object leftBracesToken extends Token
case object rightBracesToken extends Token
case object eventToken extends Token
case object actorToken extends Token
//case object boolToken extends Token
case object statemachineToken extends Token
case object initialToken extends Token
case object stateToken extends Token
case object INTEGERToken extends Token
case object BOOLToken extends Token
case object semicolonToken extends Token

object Tokenizer{
  @main def hello(): Unit = {
    print("Hello World");
  }
}

val leftBracesID: Regex = "\\{".r



def test1tokenize(input:String):Mylist[Token] = {
  val tokens = for{
    token <- input.split("\\s+")
  }yield token match{

  }
}


//********************************************
// List of reserved words
val WordList: List[String] = List("word1", "word2", "word3");

// Converting a list to a map
val ReserveMap1: Map[Int, String] = List(1 -> "words").toMap;

// Creating a map without a list
val ReserveMap2: Map[Int, String] = Map(100 -> "word", 101 -> "words");


