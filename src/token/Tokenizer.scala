package token

import scala.util.matching.Regex

class Tokenizer(code: String) {
  val tokenRegexList: List[TokenData] = createTokenRegex()
  val sourceCode: String = code
  val tokenList: List[Token] = tokenize(sourceCode)
//  val no_statements = tokenList.count(_.getToken()==";")
//  println("No of statements: "+no_statements)
  
  //creating list of all token regex
  def createTokenRegex(): List[TokenData] = {
    val _space = new TokenData(" ".r, TokenType.SPACE)
    val _break = new TokenData("""\n|;""".r, TokenType.BREAK)
    val _int_literal = new TokenData("-?\\d+".r, TokenType.INT_LITERAL)
    val _bool_literal = new TokenData("^(tt|ff)$".r, TokenType.BOOL_LITERAL)
    val _alpha_literal = new TokenData("^([\"|\']).*([\"|\'])$".r, TokenType.ALPHA_LITERAL)
    val _plus = new TokenData("\\+".r, TokenType.PLUS)
    val _multiply = new TokenData("\\*".r, TokenType.MUL)
    val _divide = new TokenData("\\/".r, TokenType.DIV)
    val _sub = new TokenData("-{1}".r, TokenType.SUB)
    val _bop = new TokenData("==|><|and|or|\\^|>|<".r, TokenType.BOP)
    val _uop = new TokenData("~|not".r, TokenType.UOP)
    val _assign_op = new TokenData("[=]{1}".r, TokenType.ASSIGNMENT)
    val _colon = new TokenData("[:]{1}".r, TokenType.COLON)
    val _while = new TokenData("(while)".r, TokenType.WHILE)
    val _do = new TokenData("(do)".r, TokenType.DO)
    val _if = new TokenData("(if)".r, TokenType.IF)
    val _then = new TokenData("(then)".r, TokenType.THEN)
    val _else = new TokenData("(else)".r, TokenType.ELSE)
    val _nil = new TokenData("(nil)".r, TokenType.NIL)
    val _print = new TokenData("(print)".r, TokenType.PRINT)
    val _data_type = new TokenData("^(int|bool|alpha)$".r, TokenType.DATA_TYPE)
    val _identifier = new TokenData("[a-zA-Z][A-Za-z0-9_$*#]*".r,TokenType.IDENTIFIER)
    val _var_type = new TokenData("(var)".r, TokenType.VAR_TYPE)
    val _const_type = new TokenData("(const)".r, TokenType.CONST_TYPE)
    val _skip = new TokenData("^(skip)$".r, TokenType.SKIP)
    val finalList = List( _break,_var_type,_const_type,_skip, _print, _while, _do, _if, _then, _else, _colon, _data_type ,_uop ,  _sub,_alpha_literal,_int_literal,_plus, _multiply, _divide, _bop,_assign_op, _bool_literal , _nil, _identifier )
    return finalList
  }
  
  //tokenize input string to a List of tokens
  def tokenize(code: String): List[Token] = {
      if (code.trim.isEmpty) List()
      else {
        val (before,token,after) = getTokens(code.trim(), tokenRegexList)
        tokenize(before) ::: List(token) ::: tokenize(after)
      }
  }
  
  //returning next token in given string
  def getTokens(code: String, tokenRegexList: List[TokenData]) : (String, Token, String) = {
    if (tokenRegexList.isEmpty) throw new Exception("Syntax Error")
    val matcher = tokenRegexList.head.getPattern().pattern.matcher(code)
    if (matcher.find()) 
      (code.substring(0,matcher.start()),new Token(code.substring(matcher.start(),matcher.end()), tokenRegexList.head.getType()),code.substring(matcher.end()))
    else
      getTokens(code,tokenRegexList.tail)
  }
  
  //get next token from given position
  def getNextToken(current_pos: Int): (Token,Int) = {
    if (current_pos > tokenList.length-1) return(new Token("EOF",null),current_pos) 
    else return (tokenList.apply(current_pos), current_pos+1)
  }
  
  def lookAhead(current_pos: Int): (Token,Int) = {
    if (current_pos > tokenList.length-1) return(new Token("EOF",null),current_pos) 
    else return (tokenList.apply(current_pos), current_pos)
  }
}