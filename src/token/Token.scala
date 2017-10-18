package token

class Token(val tokenx: String,val Typex: TokenType.Type) {
  private val token:String = tokenx;
  private val Type: TokenType.Type = Typex;
  
  def getToken() = {
    if (Type == TokenType.INTEGER) token.toInt 
    else if (Type == TokenType.BOOLEAN){ 
      if (token.equals("tt")) true
      else if (token.equals("ff")) false
    }
    token
  }
  def getType(): TokenType.Type = Type
}