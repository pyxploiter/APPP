package token
import parser._
import scala.io.Source

object main {
  def main(args: Array[String]) {
    //val print: PrintParser = new PrintParser("print \"This6656 is print parser\"")
    //print.parse()
    
    val codeFile = Source.fromFile("examples/if.appp")
    val sourceCode = codeFile.mkString
	  //println(sourceCode)  
	  val tokenizer: Tokenizer = new Tokenizer("-9*6+7/2")
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser)
    println(interpreter.interpret())
    
    //val (tok, pos) = tokenizer.getNextToken(0)
    //println(tok.getToken())
    //println(pos)
	  //tokenizer.tokenize(sourceCode).map(f => println(f.getToken() + " <= " + f.getType()))
  }
}