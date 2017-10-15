package parser
import org.junit.Assert._
import org.junit.Test

class SkipParserTest {
  
  @Test
  def isParsable(){
    val parser: SkipParser = new SkipParser("skip")
    val parser1: SkipParser = new SkipParser("skipped")
    
    assertTrue(parser.isParsable())
    assertFalse(parser1.isParsable())
  }
}