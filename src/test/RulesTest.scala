package test
import org.junit.Assert._
import org.junit.Test
import org.junit._
import scala.io.Source
import parser._
import interpreter.value
import interpreter.Interpreter
import token.Tokenizer
import token.TokenType.INT_LITERAL
import token.Token
import token.TokenType
import scala.util.Success

class RulesTest {
   val insert = new value("var","myVar","int",1337)
   val table = Map[String,value]().withDefaultValue((new value("null","null","null",0)))
   val var_table = table.+(("myVar",insert))
    
  @Test
  def allowed_datatypes_validate{
    val tokenizer: Tokenizer = new Tokenizer("")
    val parser: Parser = new Parser(tokenizer)
    parser.eat((new Token("me", TokenType.INT_LITERAL)), TokenType.INT_LITERAL, 0)  //true
    parser.eat((new Token("me", TokenType.ALPHA_LITERAL)), TokenType.ALPHA_LITERAL, 0)  //true
    parser.eat((new Token("me", TokenType.BOOL_LITERAL)), TokenType.BOOL_LITERAL, 0)  //true
    //parser.eat((new Token("me", TokenType.BOOL_LITERAL)), TokenType.BREAK, 0)  //false => program exits
  }
  
  @Test
  def bool_refers_validate{
    //true input
    val codeFile = "var x:bool = tt"
	  val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
    
    //false input
//    val codeFile1 = "var x:bool = 5"
//	  val tokenizer1: Tokenizer = new Tokenizer(codeFile1)
//    val parser1: Parser = new Parser(tokenizer1)
//    val interpreter1 = new Interpreter(parser1, var_table)
//    assertFalse(interpreter1.interpret())
  }
  
  @Test
  def int_refers_validate{
    //true input
    val codeFile = "var x:int = 100; x= ~100"
	  val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
    
    //false input
//    val codeFile1 = "var x:int = \"this is int\""
//	  val tokenizer1: Tokenizer = new Tokenizer(codeFile1)
//    val parser1: Parser = new Parser(tokenizer1)
//    val interpreter1 = new Interpreter(parser1, var_table)
//    assertFalse(interpreter1.interpret())
  }
  
  @Test
  def alpha_refers_validate{
   //true input
    val codeFile = "var x:alpha = \"this is alpha\""
	  val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
    
    //false input
//    val codeFile1 = "var x:alpha = 55"
//	  val tokenizer1: Tokenizer = new Tokenizer(codeFile1)
//    val parser1: Parser = new Parser(tokenizer1)
//    val interpreter1 = new Interpreter(parser1, var_table)
//    assertFalse(interpreter1.interpret())
  }
  
  @Test
  def identifier_validate{
    val identifier1 = "ValidName"
    val identifier2 = "Valid_Name2"
    val identifier3 = "Va#_*Na3"
    val identifier4 = "#InvalidName"
    
    val codeFile = "var "+identifier1+":int";
    val tokenizer: Tokenizer = new Tokenizer(codeFile)
    assertTrue(tokenizer.tokenList.apply(1).getToken().equals(identifier1))
  }
  
  @Test
  def expression_validate{
    val add_expr = BinOp(Num(new Token("10",TokenType.INT_LITERAL)), new Token("><", TokenType.BOP), Num(new Token("20", TokenType.INT_LITERAL)))
    val answer = true  // 10 >< 20 => 10 not equal to 20
	  val tokenizer: Tokenizer = new Tokenizer("")
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertEquals(answer, interpreter.evaluate_BinOp(add_expr, var_table)._1)
  }
  
  @Test
  def binary_operators_validate{
    val add_expr = BinOp(Num(new Token("5",TokenType.INT_LITERAL)), new Token("+", TokenType.PLUS), Num(new Token("3", TokenType.INT_LITERAL)))
    val answer = 8  // 5+3
	  val tokenizer: Tokenizer = new Tokenizer("")
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertEquals(answer, interpreter.evaluate_BinOp(add_expr, var_table)._1)
    
    val bool_equal_expr = BinOp(Bool(new Token("tt",TokenType.BOOL_LITERAL)), new Token("==", TokenType.BOP), Bool(new Token("ff", TokenType.BOOL_LITERAL)))
    val answer1 = false  // tt == ff => true == false
	  val tokenizer1: Tokenizer = new Tokenizer("")
    val parser1: Parser = new Parser(tokenizer1)
    val interpreter1 = new Interpreter(parser1, var_table)
    assertEquals(answer1, interpreter1.evaluate_BinOp(bool_equal_expr, var_table)._1)
    
    val power_of_expr = BinOp(Num(new Token("3",TokenType.INT_LITERAL)), new Token("^", TokenType.BOP), Num(new Token("3", TokenType.INT_LITERAL)))
    val answer2 = 27  // 3^3 = 3*3*3
	  val tokenizer2: Tokenizer = new Tokenizer("")
    val parser2: Parser = new Parser(tokenizer2)
    val interpreter2 = new Interpreter(parser2, var_table)
    assertEquals(answer2, interpreter2.evaluate_BinOp(power_of_expr, var_table)._1) 
  }
  
  @Test
  def unary_operators_validate{
    val not_op_expr = UnaryOp((new Token("not",TokenType.UOP)), Bool(new Token("tt", TokenType.BOOL_LITERAL)))
    val answer1 = false  // (not true = false)
	  val tokenizer1: Tokenizer = new Tokenizer("")
    val parser1: Parser = new Parser(tokenizer1)
    val interpreter1 = new Interpreter(parser1, var_table)
    assertEquals(answer1, interpreter1.evaluate_UnaryOp(not_op_expr, var_table)._1) 
    
    val neg_op_expr = UnaryOp((new Token("~",TokenType.UOP)), Num(new Token("3", TokenType.INT_LITERAL)))
    val answer2 = -3  // ~3 = -3 
	  val tokenizer2: Tokenizer = new Tokenizer("")
    val parser2: Parser = new Parser(tokenizer2)
    val interpreter2 = new Interpreter(parser2, var_table)
    assertEquals(answer2, interpreter2.evaluate_UnaryOp(neg_op_expr, var_table)._1) 
  }
  
  @Test
  def declaration_validate{
    val codeFile = """const x:int = 20;"""
	  val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
  }
  
  @Test
  def commands_validate{
    val codeFile = """var x:int = 10; while x>0 do x=x-1; print x"""
	  val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
  }
  
  @Test
  def print_call_validate{
    val statement = PrintStatement(Var(new Token("myVar", TokenType.INT_LITERAL)))
    val tokenizer2: Tokenizer = new Tokenizer("")
    val parser2: Parser = new Parser(tokenizer2)
    val interpreter2 = new Interpreter(parser2, var_table)
    assertEquals("",interpreter2.evaluate_Print(statement, var_table)._1)
  }
}