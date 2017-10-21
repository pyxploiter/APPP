package runtime
import scala.io.Source
import parser._
import interpreter.value
import interpreter.Interpreter
import token.Tokenizer

object main {
  def main(args: Array[String]) {
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0)))
    //val codeFile = Source.fromFile("examples/example2.appp").mkString
    val codeFile = """var x:bool = tt; x=not tt """
	  val tokenizer: Tokenizer = new Tokenizer(codeFile)
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    interpreter.interpret()
    
    //tokenizer.tokenize(str).map(f=> println(f.getToken()+" "+f.getType()) )
    
  }
}