import munit.Clue.generate

class LexerTest extends munit.FunSuite {
  
  test("Hello World"){
    assertEquals(Tokenizer.lexer("event hello {}") , List(
      IdentifierToken("event"),
      IdentifierToken("hello"),
      leftBracesToken,
      rightBracesToken))
  }
}
