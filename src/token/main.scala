package token
import parser._
import scala.io.Source
import parser._

object main {
  def main(args: Array[String]) {
    //val print: PrintParser = new PrintParser("print \"This6656 is print parser\"")
    //print.parse()
    val codeFile = Source.fromFile("examples/if.appp")
    val sourceCode = codeFile.mkString
	  //println(sourceCode)  
    val str:String = "var y:alpha;skip; const uzair:int=0; var asad:bool=tt; "
	  val tokenizer: Tokenizer = new Tokenizer(str)
    //tokenizer.tokenize(str).map(f=> println(f.getToken()+" "+f.getType()) )
    val parser: ExprParser = new ExprParser(tokenizer)
    val interpreter = new Interpreter(parser)
    println(interpreter.interpret())
  }
}