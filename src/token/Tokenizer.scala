package token

import scala.collection.JavaConverters._
import scala.util.matching.Regex

class Tokenizer(val strx: String) {
  //var str: String = strx.replaceAll(" ", "");
  var str: String = strx
  val lastToken: Token = new Token("", TokenType.EMPTY);
	
  val tokenList: List[TokenData] = createTokenRegex();
  
  def createTokenRegex(): List[TokenData] = {
    val tokenDatas: List[TokenData] = List[TokenData]();
    val list2 = tokenDatas.+:(new TokenData(" ".r, TokenType.SPACE));
    val list3 = list2.+:(new TokenData(";".r, TokenType.SEMICOLON));
    val list4 = list3.+:(new TokenData("-?[0-9]+".r, TokenType.INTEGER));
    val list5 = list4.+:(new TokenData("([t]{2})|([f]{2})".r, TokenType.BOOLEAN));
    val list7 = list5.+:(new TokenData("(==|><)|(and)|(or)|[\\+\\*\\/\\^><]".r, TokenType.BOP));
    val list8 = list7.+:(new TokenData("^(-|not)".r, TokenType.UOP));
    val list9 = list8.+:(new TokenData("^([\"|\']).*([\"|\'])$".r, TokenType.ALPHA));
    val list10 = list9.+:(new TokenData("nil|int|bool|alpha|not|var|const|while|do|if|then|else|skip|print".r, TokenType.RESERVED));
    val list11 = list10.+:(new TokenData("^(=)$".r, TokenType.ASSIGNMENT));
    val list12 = list11.+:(new TokenData("[:]{1}".r, TokenType.COLON));
    val list13 = list12.+:(new TokenData("^(int|bool|alpha)".r, TokenType.DATA_TYPE));
    val list14 = list13.+:(new TokenData("^[a-zA-Z]+[A-Za-z0-9_$*#]*".r,TokenType.IDENTIFIER));
    val finalList = list14.+:(new TokenData("^(const|var)".r, TokenType.VARIABLE_TYPE));  
    return finalList
  }
  
  def matchTokenPattern(pattern: Regex, string: String): String = {
    return (pattern findFirstIn(string)).getOrElse("")
  }
  
  def isVariableType(inToken: String): Boolean = {
    val token = matchTokenPattern("^(const|var)".r, inToken)  
    token == inToken
  }
  
  def isDataType(inToken: String): Boolean = {
    val token = matchTokenPattern("^(int|bool|alpha)".r, inToken)
    token == inToken
  }
  
  def isInteger(inToken: String): Boolean = {
    val token = matchTokenPattern("-?[0-9]+".r, inToken)  
    token == inToken
  }
  
  def isBoolean(inToken: String): Boolean = {
    val token = matchTokenPattern("([t]{2})|([f]{2})".r, inToken)  
    token == inToken
  }
  
  def isAlpha(inToken: String): Boolean = {
    val token = matchTokenPattern("^([\"|\']).*([\"|\'])$".r, inToken)  
    token == inToken
  }
  
//  def isString(inToken: String): Boolean = {
//    val token = matchTokenPattern("\".*\"".r, inToken)  
//    token == inToken
//  }
  
  def isIdentifier(inToken: String): Boolean = {
    if(isReserved(inToken)){
      return false
    }
    val token = matchTokenPattern("^[a-zA-Z]+[A-Za-z0-9_$*#]*".r, inToken)  
    token == inToken
  }
  
  def isUOP(inToken: String): Boolean = {
    val token = matchTokenPattern("^(-|not)".r, inToken)  
    token == inToken
  }
  
  def isBOP(inToken: String): Boolean = {
    val token = matchTokenPattern("(==|><|and|or)|[\\+\\*\\/\\^><]".r, inToken)  
    token == inToken
  }
  
  def isReserved(inToken: String): Boolean = {
    val token = matchTokenPattern("nil|int|bool|alpha|not|var|const|while|do|if|then|else|skip|print".r, inToken)  
    token == inToken
  }
  
  def nextToken() : Token = {
    for(data <- tokenList){
      //println(data.getPattern())
      val token = matchTokenPattern(data.getPattern(), str)
      //println(str); 
      str = str.replace(token, "")
      if (!token.equals("")){
        
        //println(data.getType()+" "+token)
        if (data.getType().equals(TokenType.ALPHA)){
          return(new Token(token.substring(1, token.length()-1), data.getType()))
        }
        
        if (data.getType().equals(TokenType.IDENTIFIER)){
          if(isDataType(token)) return(new Token(token, TokenType.DATA_TYPE))
          if(isReserved(token)) return(new Token(token, TokenType.RESERVED))
        }
        return (new Token(token, data.getType()))
      }
     }
    return lastToken
  }
  
	def hasNextToken(): Boolean = {
		return !str.isEmpty();
	}
}