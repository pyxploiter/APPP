package token

class Token(val tokenx: String,val Typex: TokenType.EnumVal) {
  private val token:String = tokenx;
  private val Type: TokenType.EnumVal = Typex;
  
  def getToken(): String = token
  def getType(): TokenType.EnumVal = Type
}