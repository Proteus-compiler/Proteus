import munit.Clue.generate

class LexerTest extends munit.FunSuite {
  
  test("Hello World"){
    assertEquals(Tokenizer.lexer("event hello {}") , List(
      eventToken,
      IdentifierToken("hello"),
      leftBracesToken,
      rightBracesToken
    ))
  }

  test("5+5"){
    assertEquals(Tokenizer.lexer("5+5"), List(
      IntegerLiteralToken(5),
      plusToken,
      IntegerLiteralToken(5)
    ))
  }

  test("comments"){
    assertEquals(Tokenizer.lexer("//This is a comment"), List())
  }
}
