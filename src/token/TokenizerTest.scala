package token
import org.junit.Assert._
import org.junit.Test

class TokenizerTest {
  
  @Test
  def isVariableTypeTest(){
    val tokenizer: Tokenizer = new Tokenizer("")
    
    assertFalse(tokenizer.isVariableType("variable"))
    assertTrue(tokenizer.isVariableType("const"))
    assertTrue(tokenizer.isVariableType("var"))
  }
  
  @Test
  def isDataTypeTest(){
    val tokenizer: Tokenizer = new Tokenizer("")
    
    assertFalse(tokenizer.isDataType("variable"))
    assertTrue(tokenizer.isDataType("int"))
    assertTrue(tokenizer.isDataType("alpha"))
    assertFalse(tokenizer.isDataType("boolean"))
  }
  
  @Test
  def isIntegerTest(){
    val tokenizer: Tokenizer = new Tokenizer("")
    
    assertFalse(tokenizer.isInteger("1337Alpha"))
    assertTrue(tokenizer.isInteger("-234"))
    assertTrue(tokenizer.isInteger("463"))
    assertFalse(tokenizer.isInteger("463-"))
  }
  
  @Test
  def isBooleanTest(){
    val tokenizer: Tokenizer = new Tokenizer("")
    
    assertFalse(tokenizer.isBoolean("ttt"))
    assertTrue(tokenizer.isBoolean("tt"))
    assertTrue(tokenizer.isBoolean("ff"))
    assertFalse(tokenizer.isBoolean("ttff"))
  }
  
  @Test
  def isAlphaTest(){
    val tokenizer: Tokenizer = new Tokenizer("")

    assertFalse(tokenizer.isAlpha("\"1337Alpha"))
    assertTrue(tokenizer.isAlpha("\"1337Alpha4ali\""))
    assertTrue(tokenizer.isAlpha("'asad.ali'"))
    assertFalse(tokenizer.isAlpha("46A\""))
  }
  
//  @Test
//  def isStringTest(){
//    val tokenizer: Tokenizer = new Tokenizer("")
//
//    assertFalse(tokenizer.isAlpha("\"1337Alpha"))
//    assertTrue(tokenizer.isAlpha("\"Alpha34li\""))
//    assertFalse(tokenizer.isAlpha("463s\""))
//  }
  
  @Test
  def isIdentifierTest(){
    val tokenizer: Tokenizer = new Tokenizer("")
    
    assertTrue(tokenizer.isIdentifier("var_3#"))
    assertFalse(tokenizer.isIdentifier("3identity"))
    assertFalse(tokenizer.isIdentifier("-3ThisIS"))
    assertFalse(tokenizer.isIdentifier("print"))    //reserved
    assertTrue(tokenizer.isIdentifier("meraVariab#"))
  }
    
  @Test
  def isUOPTest(){
    val tokenizer: Tokenizer = new Tokenizer("")

    assertTrue(tokenizer.isUOP("-"))
    assertTrue(tokenizer.isUOP("not"))
    assertFalse(tokenizer.isUOP("-not"))
    assertFalse(tokenizer.isUOP("nots-"))
  }
  
  @Test
  def isBOPTest(){
   val tokenizer: Tokenizer = new Tokenizer("")

    assertTrue(tokenizer.isBOP("><"))
    assertTrue(tokenizer.isBOP("=="))
    assertTrue(tokenizer.isBOP(">"))
    assertTrue(tokenizer.isBOP("<"))
    assertTrue(tokenizer.isBOP("/"))
    assertTrue(tokenizer.isBOP("*"))
    assertTrue(tokenizer.isBOP("^"))
    assertTrue(tokenizer.isBOP("+"))
    assertTrue(tokenizer.isBOP("or"))
    assertTrue(tokenizer.isBOP("and"))
    
    assertFalse(tokenizer.isBOP("<>"))
    assertFalse(tokenizer.isBOP(">="))
    assertFalse(tokenizer.isBOP("++"))
    assertFalse(tokenizer.isBOP("/*")) 
  }
  
  @Test
  def isReservedTest(){
    val tokenizer: Tokenizer = new Tokenizer("")
    assertTrue(tokenizer.isReserved("if"))
    assertTrue(tokenizer.isReserved("do"))
    assertTrue(tokenizer.isReserved("print"))
    assertTrue(tokenizer.isReserved("then"))
    assertTrue(tokenizer.isReserved("const"))
    
    assertFalse(tokenizer.isReserved("ifo"))
    assertFalse(tokenizer.isReserved("constant"))
    assertFalse(tokenizer.isReserved("variable"))
    assertFalse(tokenizer.isReserved("Print"))
  }
}