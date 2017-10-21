package test
import org.junit.Assert._
import org.junit.Test
import org.junit._
import token.Tokenizer

class TokenizerTest {
  val tokenizer: Tokenizer = new Tokenizer("")
  @Test
  def tokenizeTest{
    val input = "var x:int = 10"
    val true_expected = List("var","x",":","int","=","10")
    val false_expected = List("const","x",":","int","+","11") 
    val tokens = tokenizer.tokenize(input)
    val result = tokens.map(_.getToken())
    assertEquals(true_expected, result)  
    assertNotEquals(false_expected, result)
  } 
  
  @Test
  def getNextTokenTest{
    val input = "const x:int = 5"
    val true_expected = List("const","x",":","int","=","5")
    val last_token = tokenizer.getNextToken(true_expected.length)._1.getToken()
    assertEquals(last_token, "EOF")
  }
}