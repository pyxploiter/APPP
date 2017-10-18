package token

class Token(val tokenx: String,val Typex: TokenType.Type) {
  private val token:String = tokenx;
  private val Type: TokenType.Type = Typex;
  
  def getToken() = token
  def getType(): TokenType.Type = Type
}