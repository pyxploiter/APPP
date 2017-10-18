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
	  val tokenizer: Tokenizer = new Tokenizer("-4+3*2")
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser)
    println(interpreter.interpret())
    //println(tokenizer.tokenList.map(println(_)))
  }
}