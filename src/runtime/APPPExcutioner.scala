package runtime
import scala.io.Source
import parser._
import interpreter.value
import interpreter.Interpreter
import token.Tokenizer

object main {
  def main(args: Array[String]) {
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0)))
    //val codeFile = Source.fromFile("examples/if.appp").mkString
	  //println(sourceCode)  
    val str:String = "var x:int = 3; x=5; if x then print x else print \"false\";";
	  val tokenizer: Tokenizer = new Tokenizer(str)
    //tokenizer.tokenize(str).map(f=> println(f.getToken()+" "+f.getType()) )
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    interpreter.interpret()
  }
}