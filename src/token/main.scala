package token
import parser._
import scala.io.Source

object main {
  def main(args: Array[String]) {
    var parse: Parser = new PrintParser("print '-Xthis'")
    println(parse.isParsable)
    val codeFile = Source.fromFile("examples/if.appp")
    val sourceCode = codeFile.mkString
    println(sourceCode)
	  //val code: String = "var meraVariab#:alpha = \"-23\" -23; const and Meta35_s:bool = 23"
	  val statement = sourceCode.split("[\\n]{1}");
	  for(v <- statement){
	    //println(v.trim())  
	    val tokenizer: Tokenizer = new Tokenizer(v.trim());
	    //while(tokenizer.hasNextToken()){
	      println(tokenizer.nextToken().getToken())
      //}
	  }
  }
}