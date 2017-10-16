package token

import scala.collection.JavaConverters._
import scala.util.matching.Regex

class Tokenizer() {
  //var str: String = strx.replaceAll(" ", "");
  //var str: String = strx
  val lastToken: Token = new Token("", TokenType.EMPTY);
	
  val tokenList: List[TokenData] = createTokenRegex();
  
  def createTokenRegex(): List[TokenData] = {
    val _space = new TokenData(" ".r, TokenType.SPACE);
    val _break = new TokenData("\\n|;".r, TokenType.BREAK);
    val _int_literal = new TokenData("-?[0-9]+".r, TokenType.INTEGER);
    val _bool_literal = new TokenData("tt|ff".r, TokenType.BOOLEAN);
    val _alpha_literal = new TokenData("^([\"|\']).*([\"|\'])$".r, TokenType.ALPHA);
    val _bop = new TokenData("(==|><)|(and)|(or)|[\\+\\*\\/\\^><]".r, TokenType.BOP);
    val _uop = new TokenData("^(-|not)".r, TokenType.UOP);
    val _assign_op = new TokenData("[=]{1}".r, TokenType.ASSIGNMENT);
    val _colon = new TokenData("[:]{1}".r, TokenType.COLON);
    val _while = new TokenData("while".r, TokenType.WHILE);
    val _do = new TokenData("do".r, TokenType.DO);
    val _if = new TokenData("if".r, TokenType.IF);
    val _then = new TokenData("then".r, TokenType.THEN);
    val _else = new TokenData("else".r, TokenType.ELSE);
    val _nil = new TokenData("nil".r, TokenType.NIL);
    val _print = new TokenData("print".r, TokenType.PRINT);
    val _data_type = new TokenData("int|bool|alpha".r, TokenType.DATA_TYPE);
    val _identifier = new TokenData("[a-zA-Z][A-Za-z0-9_$*#]*".r,TokenType.IDENTIFIER);
    val _variable_type = new TokenData("const|var".r, TokenType.VARIABLE_TYPE);
    val finalList = List(_break,_variable_type, _print, _while, _do, _if, _then, _else, _colon, _data_type, _assign_op, _uop, _bop, _alpha_literal, _bool_literal, _int_literal, _identifier )
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
  
  def tokenize(code: String): List[Token] = {
      if (code.trim().isEmpty)
        List()
      else{
        val (before,token,after) = nextToken(code.trim(), tokenList)
        //println(token.getToken()+" "+token.getType())
        tokenize(before) ::: List(token) ::: tokenize(after)
      }
  }
  
  def nextToken(code: String, tokenList: List[TokenData]) : (String, Token, String) = {
    if (tokenList.isEmpty) throw new Exception("No match found")
    val regex = tokenList.head.getPattern()
    val tokentype = tokenList.head.getType()
    
    val matcher = regex.pattern.matcher(code)
    //println(code)
    if (matcher.find()){
      (
        code.substring(0,matcher.start()),
        new Token(code.substring(matcher.start(),matcher.end()), tokentype),
        code.substring(matcher.end())
      )
    }else
      nextToken(code,tokenList.tail)
  }
//    for(data <- tokenList){
//      //println(data.getPattern())
//      val token = matchTokenPattern(data.getPattern(), str)
//      //println(str); 
//      str = str.replace(token, "")
//      if (!token.equals("")){
//        
//        println(data.getType()+" "+token)
//        if (data.getType().equals(TokenType.ALPHA)){
//          return(new Token(token.substring(1, token.length()-1), data.getType()))
//        }
//        
//        if (data.getType().equals(TokenType.IDENTIFIER)){
//          //if(isDataType(token)) return(new Token(token, TokenType.DATA_TYPE))
//          //if(isReserved(token)) return(new Token(token, TokenType.RESERVED))
//        }
//        return (new Token(token, data.getType()))
//      }
//     }
//    return lastToken
//  }
}