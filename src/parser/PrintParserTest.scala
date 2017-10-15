package parser
import org.junit.Assert._
import org.junit.Test

class PrintParserTest {
  
  @Test
  def isParsable(){
    val parser: PrintParser = new PrintParser("print this")
    val parser1: PrintParser = new PrintParser("print 'HELLO, WORld$$'")
    
    assertFalse(parser.isParsable())
    assertTrue(parser1.isParsable())
  }
}