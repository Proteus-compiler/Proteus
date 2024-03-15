import org.scalatest.funsuite.AnyFunSuite
class LexerTest extends AnyFunSuite {
  
  test("Hello World"){
    assert(Tokenizer.lexer("event hello {}") === List(
      IdentifierToken("event"),
      IdentifierToken("hello"),
      leftBracesToken,
      rightBracesToken))
  }
}
