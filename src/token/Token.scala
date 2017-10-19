package token

class Token(val tokenx: String,val Typex: TokenType.Type) {
  private val token:String = tokenx;
  private val Type: TokenType.Type = Typex;
  
  def getToken() = token
  def getOriginalToken():String = {
    if (Type == TokenType.BOOL_LITERAL){
      if (token.equals("tt")) "true"
      else "false"
    } else token
  }
  def getType(): TokenType.Type = Type
}