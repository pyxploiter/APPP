package parser
import org.junit.Assert._
import org.junit.Test

class IfStatementParserTest {
  
  @Test
  def isParsable(){
    val parser: IfStatementParser = new IfStatementParser("if ff then fuck else off")
    val parser1: IfStatementParser = new IfStatementParser("if tt then print this else tt")
    
    assertTrue(parser.isParsable())
    assertFalse(parser.isParsable())
  }
}