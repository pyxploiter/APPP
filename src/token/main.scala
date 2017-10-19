package token
import parser._
import scala.io.Source
import parser._

object main {
  def main(args: Array[String]) {
    val codeFile = Source.fromFile("examples/if.appp")
    val sourceCode = codeFile.mkString
	  //println(sourceCode)  
    val str:String = "var x:int=6"
	  val tokenizer: Tokenizer = new Tokenizer(str)
    //tokenizer.tokenize(str).map(f=> println(f.getToken()+" "+f.getType()) )
    val parser: ExprParser = new ExprParser(tokenizer)
    
    val id_value = new value("var", "int","x", 1)
    val var_table = Map("x"->id_value).withDefaultValue((new value("null","null","null","null")))
    
    val interpreter = new Interpreter(parser, var_table)
    println(interpreter.interpret())
  }
}