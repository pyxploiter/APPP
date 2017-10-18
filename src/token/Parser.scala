package token

trait AST
case class BinOp(val left: AST, val token: Token, val right: AST) extends AST
case class UnaryOp(val token: Token, expr_node: AST) extends AST
case class Num(val token:Token) extends AST
//case class Bool(val token:Token) extends AST

class Parser(val tokenizer: Tokenizer) {
  val (cr_token, cr_token_pos): (Token,Int) = tokenizer.getNextToken(0)
  
  def eat(current_token: Token, token_type: TokenType.Type, current_token_pos:Int): (Token, Int) = {
    if (current_token.getType() == token_type){ 
      return tokenizer.getNextToken(current_token_pos)
    }
   else throw new Exception("Eat Error")
  }
  
  def factor(current_token: Token, current_token_pos:Int): (Token, Int, AST) = {
    if (current_token.getType() == TokenType.UOP){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.UOP, current_token_pos)
      val (cur_tok, cur_pos, node) = factor(cur_token, cur_token_pos)
      return (cur_tok, cur_pos, UnaryOp(current_token, node))
    }
    
    else if(current_token.getType() == TokenType.INTEGER){
      val (cur_token, cur_token_pos) = eat(current_token, TokenType.INTEGER, current_token_pos)
      return (cur_token, cur_token_pos, Num(current_token))
    }
    
//    else if(current_token.getType() == TokenType.BOOLEAN){
//      val (cur_token, cur_token_pos) = eat(current_token, TokenType.BOOLEAN, current_token_pos)
//      return (cur_token, cur_token_pos, Bool(current_token))
//    }
    
    else throw new Exception("factor Error")
  }
  
  def term(current_token: Token, current_token_pos:Int): (Token, Int, AST) = {
    val (cur_token, cur_token_pos, left_node) = factor(current_token, current_token_pos)
    
    def recurse_term(cur_token: Token, cur_token_pos: Int, left_node: AST): (Token,Int,AST) = { 
      if (cur_token.getType() == TokenType.MUL){
        val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.MUL, cur_token_pos)
        val (tok,pos,right_node) = factor(cur_tok, cur_tok_pos)
        val next_node = BinOp(left_node, cur_token, right_node) 
        recurse_term(tok, pos, next_node)
      }
      else if (cur_token.getType() == TokenType.DIV){
        val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.DIV,cur_token_pos)
        val (tok,pos,right_node) = factor(cur_tok, cur_tok_pos)
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
  
  def parse(): AST = {
    val (tok,pos,tree) = expr(cr_token, cr_token_pos)
    println(tree)
    return tree
  }
}