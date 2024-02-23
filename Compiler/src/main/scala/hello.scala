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

}

val leftBracesID: Regex = "\\{".r



def test1tokenize(input:String):Mylist[Token] = {
  val tokens = for{
    token <- input.split("\\s+")
  }yield token match{

  }
}
object hello{

  @main def howdy(): Unit =
    print("Hello World!")

}