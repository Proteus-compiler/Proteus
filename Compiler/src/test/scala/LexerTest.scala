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
      addToken,
      IntegerLiteralToken(5)
    ))
  }

  test("integer"){
    assertEquals(Tokenizer.lexer("          int num = 7;"),List(
      intToken,
      IdentifierToken("num"),
      assignToken,
      IntegerLiteralToken(7),
      semicolonToken
    ))
  }

  test("comments"){
    assertEquals(Tokenizer.lexer(""), List())
  }

  test("comments with newline"){
    assertEquals(Tokenizer.lexer("// This is a comment\nint num = 7;"), List(
      intToken,
      IdentifierToken("num"),
      assignToken,
      IntegerLiteralToken(7),
      semicolonToken
    ))
  }
  test("new line"){
    assertEquals(Tokenizer.lexer("\n int num = 7;"), List(
      intToken,
      IdentifierToken("num"),
      assignToken,
      IntegerLiteralToken(7),
      semicolonToken
    ))
  }
  test("empty string"){
    assertEquals(Tokenizer.lexer("int "), List(
      intToken
    ))
  }

  test("underscore") {
    assertEquals(Tokenizer.lexer("int _name_number = 7"), List(
      intToken,
      IdentifierToken("_name_number"),
      assignToken,
      IntegerLiteralToken(7)
    ))
  }


  test("underscore_1") {
    assertEquals(Tokenizer.lexer("int _ = 7"), List(
      intToken,
      IdentifierToken("_"),
      assignToken,
      IntegerLiteralToken(7)
    ))
  }

  test("unfinished string") {
    assertEquals(Tokenizer.lexer("\" this is unfinished "), List())
  }

}
