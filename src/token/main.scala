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
	  val tokenizer: Tokenizer = new Tokenizer("var x=13; var y=x")
    val parser: ExprParser = new ExprParser(tokenizer)
    val interpreter = new Interpreter(parser)
    println(interpreter.interpret())
    //tokenizer.tokenize(sourceCode).map(f=> println(f.getToken()) )
  }
}