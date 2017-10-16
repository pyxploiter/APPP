package token
import parser._
import scala.io.Source

object main {
  def main(args: Array[String]) {
    val codeFile = Source.fromFile("examples/if.appp")
    val sourceCode = codeFile.mkString
    //println(sourceCode)
	  //val code: String = "var meraVariab#:alpha = \"-23\" -23; const and Meta35_s:bool = 23"
	  val statement = sourceCode.split("[\\n]{1}");
	  //for(v <- statement){
	    println(sourceCode)  
	    val tokenizer: Tokenizer = new Tokenizer();
	    tokenizer.tokenize(sourceCode).map(f => println(f.getToken()))
	      //val my = tokenizer.nextToken()
	      //println(my.getToken()+" "+my.getType())
	  //}
  }
}