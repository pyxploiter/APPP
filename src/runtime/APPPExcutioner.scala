package runtime
import scala.io.Source
import parser._
import interpreter.value
import interpreter.Interpreter
import token.Tokenizer

object main {
  def main(args: Array[String]) {
    val codeFile = Source.fromFile("examples/if.appp").mkString
	  //println(sourceCode)  
    val str:String = """var x:int=nil;
const y:int=2;
x = y+1
print x
""";
	  val tokenizer: Tokenizer = new Tokenizer(str)
    tokenizer.tokenize(str).map(f=> println(f.getToken()+" "+f.getType()) )
    val parser: Parser = new Parser(tokenizer)
    
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0)))
    
    val interpreter = new Interpreter(parser, var_table)
    interpreter.interpret()
  }
}