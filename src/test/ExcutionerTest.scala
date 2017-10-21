package test
import org.junit.Assert._
import org.junit.Test
import org.junit._
import scala.io.Source
import parser._
import interpreter.value
import interpreter.Interpreter
import token.Tokenizer

class ExcutionerTest {  
  @Test
  def AssignmentTest1{ 
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0))) 
    val codeFile = Source.fromFile("examples/example1.appp").mkString
    val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
  }
  
  @Test
  def AssignmentTest2{ 
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0))) 
    val codeFile = Source.fromFile("examples/example2.appp").mkString
    val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
  }
  
  @Test
  def AssignmentTest3{ 
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0))) 
    val codeFile = Source.fromFile("examples/example3.appp").mkString
    val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
  }
  
  @Test
  def AssignmentTest4{ 
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0))) 
    val codeFile = Source.fromFile("examples/example4.appp").mkString
    val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
  }
  
  @Test
  def extraTests{
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0))) 
    val codeFile = "var i:int = 10; while i><0 do i=i-1; print i"
    val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    assertTrue(interpreter.interpret())
  }
}