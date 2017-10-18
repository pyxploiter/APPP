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
	  val tokenizer: Tokenizer = new Tokenizer("3+7/3+6*3*5/8");
    val parser: Parser = new Parser(tokenizer)
    parser.parse()
    //val (tok, pos) = tokenizer.getNextToken(0)
    //println(tok.getToken())
    //println(pos)
	  //tokenizer.tokenize(sourceCode).map(f => println(f.getToken() + " <= " + f.getType()))
  }
}