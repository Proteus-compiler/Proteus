import org.junit.Assert._
import org.junit.Test
import src.main.scala.*
class LexerTest {

  @Test
  def helloTest(): Unit = {
    val test = lexer("hello")
    val token = List[Tokens](IdentifierToken(hello))
    assertEquals( test, token )
  }

  @Test
  def subtractionTest(): Unit = {
    assertEquals(2, 5 - 3)
  }

}
