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

  test("integer"){
    assertEquals(Tokenizer.lexer("          int num = 7;"),List(
      intToken,
      IdentifierToken("num"),
      singleEqualsToken,
      IntegerLiteralToken(7),
      semicolonToken
    ))
  }

  test("comments"){
    assertEquals(Tokenizer.lexer("//This is a comment"), List())
  }

  test("comments with newline"){
    assertEquals(Tokenizer.lexer("//This is a comment\nint num = 7;"), List(
      intToken,
      IdentifierToken("num"),
      singleEqualsToken,
      IntegerLiteralToken(7),
      semicolonToken
    ))
  }
  test("new line"){
    assertEquals(Tokenizer.lexer("\n int num = 7;"), List(
      intToken,
      IdentifierToken("num"),
      singleEqualsToken,
      IntegerLiteralToken(7),
      semicolonToken
    ))
  }
}
