package parser
import token.Token
import token.TokenType
import token.Tokenizer

class ParserTest {
  val str:String = "var x:int = 3; x=5; if x then print x else print \"false\";";
  val tokenizer: Tokenizer = new Tokenizer(str)
  val parser: Parser = new Parser(tokenizer)
  
}