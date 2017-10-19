package parser

import token.Token
import token.TokenType
import token.Tokenizer
import scala.util.{ Try, Success, Failure }

//type Environment = String => Int
trait AST

case class BinOp(val left: AST, val token: Token, val right: AST) extends AST
case class UnaryOp(val token: Token, expr_node: AST) extends AST
case class Num(val token:Token) extends AST
case class Bool(val token:Token) extends AST
case class Alpha(val token:Token) extends AST
case class Assign(val left:AST, val token: Token, val right: AST) extends AST
case class Var(val token: Token) extends AST
case class Nil(val token:Token) extends AST
case class Const(val token: Token) extends AST
case class Compound(val children: List[AST]) extends AST

class ExprParser(val tokenizer: Tokenizer) {
  val (cr_token, cr_token_pos): (Token,Int) = tokenizer.getNextToken(0)
  
  def eat(current_token: Token, token_type: TokenType.Type, current_token_pos:Int): (Token, Int) = {
    if (current_token.getType() == token_type){ 
      return tokenizer.getNextToken(current_token_pos)
    }
   else throw new Exception("Eat Error")
  }
  
  //compound statement is actually root of parse tree
  def compound_statement(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
    val (cur_token, cur_token_pos, nodes) = statement_list(current_token, current_token_pos)
    val root = Compound(nodes)
    return (cur_token, cur_token_pos,root)
  }
  
  //returning list of statement nodes
  def statement_list(current_token:Token, current_token_pos:Int): (Token, Int, List[AST]) = {
    val (cur_token, cur_token_pos, node) = statement(current_token, current_token_pos)
    val results = List(node)
    
    def get_next_statement(token: Token, token_pos: Int, resultx: List[AST]): (Token, Int, List[AST]) = {
      if (token.getType() == TokenType.BREAK){
        val (cur_token, cur_token_pos) = eat(token, TokenType.BREAK,token_pos)
        if (cur_token_pos != token_pos){
          val (cur_tok, cur_tok_pos, node) = statement(cur_token, cur_token_pos)
          val new_list: List[AST] = resultx :+ node
          val (tok, pos, result) = get_next_statement(cur_tok, cur_tok_pos, new_list)
          return (tok, pos, result)
        }
        else
          return (token, token_pos, resultx)  //if statements are ended
      } else{
        if (token_pos == tokenizer.tokenList.length){  //if statements are ended
          return (token, token_pos, resultx)
        }
        else{
          throw new Exception("Error: Semi colon missing") //if semicolon is missing
        }
      }
    }
    
    val (cur_tok, cur_tok_pos, new_results) = get_next_statement(cur_token, cur_token_pos, results)
    return (cur_tok, cur_tok_pos, new_results)
  }
  
  //returning single statement node
  def statement(current_token:Token, current_token_pos:Int): (Token, Int, AST) = {
    if (current_token.getType() == TokenType.VARIABLE_TYPE){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.VARIABLE_TYPE,current_token_pos)
      val (tok,pos,node) = delare_statement(cur_token, cur_token_pos)
      return (tok,pos,node)
    }
    
    else if (current_token.getType() == TokenType.SKIP){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.SKIP,current_token_pos)
      val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.BREAK,cur_token_pos)
      
      def skip_statement(tokenx:Token, positionx:Int): (Token,Int) = {
        if(tokenx.getType() == TokenType.BREAK)
          return (tokenx,positionx)
        else{
          val (cur_tok, cur_tok_pos) = eat(tokenx, tokenx.getType(),positionx)
          skip_statement(cur_tok, cur_tok_pos)
        }
      }
      
      val (token, position) = skip_statement(cur_tok, cur_tok_pos)
      val (tok, pos) = eat(token, TokenType.BREAK,position)
      statement(tok,pos)
    }
    
    else throw new Exception("No more statements, Probably program ended")
  }
  
  //returning atomic value
  def atom(current_token: Token, current_token_pos:Int): (Token, Int, AST) = {
    if (current_token.getType() == TokenType.UOP){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.UOP, current_token_pos)
      val (cur_tok, cur_pos, node) = atom(cur_token, cur_token_pos)
      return (cur_tok, cur_pos, UnaryOp(current_token, node))
    }
    
    else if(current_token.getType() == TokenType.INT_LITERAL){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.INT_LITERAL, current_token_pos)
      return (cur_token, cur_token_pos, Num(current_token))
    }
    
    else if(current_token.getType() == TokenType.BOOL_LITERAL){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.BOOL_LITERAL, current_token_pos)
      return (cur_token, cur_token_pos, Bool(current_token))
    }
    
    else if(current_token.getType() == TokenType.ALPHA_LITERAL){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.ALPHA_LITERAL, current_token_pos)
      return (cur_token, cur_token_pos, Alpha(current_token))
    }
    
    else if(current_token.getType() == TokenType.NIL){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.NIL, current_token_pos)
      return (cur_token, cur_token_pos, Nil(current_token))
    }
    
    else{
     val (cur_token, cur_token_pos, node) = variable(current_token, current_token_pos)
     return (cur_token, cur_token_pos, node)
    }
    
//    else if(current_token.getType() == TokenType.BOOLEAN){
//      val (cur_token, cur_token_pos) = eat(current_token, TokenType.BOOLEAN, current_token_pos)
//      return (cur_token, cur_token_pos, Bool(current_token))
//    }
  }
  
  def term(current_token: Token, current_token_pos:Int): (Token, Int, AST) = {
    val (cur_token, cur_token_pos, left_node) = atom(current_token, current_token_pos)
    
    def recurse_term(cur_token: Token, cur_token_pos: Int, left_node: AST): (Token,Int,AST) = { 
      if (cur_token.getType() == TokenType.MUL){
        val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.MUL, cur_token_pos)
        val (tok,pos,right_node) = atom(cur_tok, cur_tok_pos)
        val next_node = BinOp(left_node, cur_token, right_node) 
        recurse_term(tok, pos, next_node)
      }
      else if (cur_token.getType() == TokenType.DIV){
        val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.DIV,cur_token_pos)
        val (tok,pos,right_node) = atom(cur_tok, cur_tok_pos)
        val next_node = BinOp(left_node, cur_token, right_node)
        recurse_term(tok, pos, next_node)
      }
      else{
        return (cur_token, cur_token_pos, left_node)
      }
    }
    
    val (tok, pos, node) = recurse_term(cur_token, cur_token_pos, left_node) 
    return (tok,pos,node)
  }
  
  def expr(current_token: Token, current_token_pos:Int): (Token, Int, AST) = {
    val (cur_token, cur_token_pos, left_AST) = term(current_token, current_token_pos)
    def recurse_expr(cur_token: Token, cur_token_pos: Int, left_AST: AST): (Token,Int,AST) = { 
      if (cur_token.getType() == TokenType.PLUS){
        val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.PLUS, cur_token_pos)
        val (tok,pos,right_bin_op) = term(cur_tok, cur_tok_pos)
        val next_node = BinOp(left_AST, cur_token, right_bin_op)
        recurse_expr(tok, pos, next_node)
      }
      else (cur_token,cur_token_pos,left_AST)
    }
    val (tok, pos, node) = recurse_expr(cur_token, cur_token_pos, left_AST) 
    return (tok,pos,node)
  }
  
  //parsing declaration statements
  def delare_statement(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
    val (cur_token, cur_token_pos,left_node) = variable(current_token, current_token_pos)
    val (current_tok, current_tok_pos) = eat(cur_token, TokenType.COLON,cur_token_pos)
    val data_type_token = current_tok
    val (cur_tok, cur_tok_pos) = eat(current_tok, TokenType.DATA_TYPE,current_tok_pos)
    if(cur_tok.getType() != TokenType.ASSIGNMENT){
      if (cur_tok.getType() == TokenType.BREAK){
        val next_node = Assign(left_node, cur_token, null)
        return (cur_tok, cur_tok_pos, next_node)
      }
      else throw new Exception("Error: Invalid Assignment Statement")
    }
    val (c_tok, tok_pos) = eat(cur_tok, TokenType.ASSIGNMENT,cur_tok_pos)
    if (data_type_token.getToken().equals("int")){
      Try(c_tok.getToken().toInt).getOrElse(
          if(c_tok.getType() == TokenType.NIL) Success
          else if(c_tok.getType() == TokenType.IDENTIFIER) Success
          else throw new Exception("Error: Value should be Integer")
       )
    } else if (data_type_token.getToken().equals("bool")){
      Try(c_tok.getOriginalToken().toBoolean).getOrElse(
          if(c_tok.getType() == TokenType.NIL) Success
          else if(c_tok.getType() == TokenType.IDENTIFIER) Success
          else throw new Exception("Error: Value should be Boolean")
      )
    } else if (data_type_token.getToken().equals("alpha")){
      if (c_tok.getOriginalToken().charAt(0).!=('\"')){
          if(c_tok.getType() == TokenType.NIL) Success
          else if(c_tok.getType() == TokenType.IDENTIFIER) Success
          else throw new Exception("Error: Value should be Alpha")
      }
    }
    
    val (tok, pos, right_node) = expr(c_tok, tok_pos)
    val next_node = Assign(left_node, cur_token, right_node)
    return (tok, pos, next_node)
  }
  
  def variable(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
    if (tokenizer.tokenList.apply(current_token_pos-2).getToken().equals("var")){
      val node = Var(current_token)
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.IDENTIFIER, current_token_pos)
      return (cur_token, cur_token_pos, node)
    }
    else if (tokenizer.tokenList.apply(current_token_pos-2).getToken().equals("const")){ 
      val node = Const(current_token)
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.IDENTIFIER, current_token_pos)
      return (cur_token, cur_token_pos, node)
    }
    else throw new Exception("Error: Invalid Variable type")
  }
  
  def parse(): AST = {
    //val (tok,pos,tree) = expr(cr_token, cr_token_pos)
    val (tok,pos,tree) = compound_statement(cr_token, cr_token_pos)
    println(tree) 
    return tree
  }
}