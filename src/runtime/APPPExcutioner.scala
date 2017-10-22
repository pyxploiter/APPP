package runtime
import scala.io.Source
import parser._
import interpreter.value
import interpreter.Interpreter
import token.Tokenizer

object main {
  def main(args: Array[String]) {
    val var_table = Map[String,value]().withDefaultValue((new value("null","null","null",0)))
    val codeFile = Source.fromFile("examples/example6.appp").mkString
    //val codeFile = """var x:int = 10; while x>0 do x=x-1; skip; print x; print x+10"""
    val tokenizer: Tokenizer = new Tokenizer(codeFile)
    //tokenizer.tokenize(codeFile).map(f => println(f.getToken()+" "+f.getType()))
    val parser: Parser = new Parser(tokenizer)
    val interpreter = new Interpreter(parser, var_table)
    interpreter.interpret()
  }
}