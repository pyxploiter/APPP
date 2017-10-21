package parser

import token.Token
import token.TokenType
import token.Tokenizer
import scala.util.{ Try, Success, Failure }

class Parser(val tokenizer: Tokenizer) {
  val (cr_token, cr_token_pos): (Token,Int) = tokenizer.getNextToken(0)
  
  //removing the current token which has been used
  def eat(current_token: Token, token_type: TokenType.Type, current_token_pos:Int): (Token, Int) = {
    try{
      if (current_token.getType() == token_type)
        return tokenizer.getNextToken(current_token_pos)
      else throw new Exception()
    } catch {case ex:Exception => println("Error: Syntax Error"); exit}
  }
  
  //AbstractSyntaxTree statement is actually root of parse tree
  def compound_statement(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
    val (cur_token, cur_token_pos, nodes) = statement_list(current_token, current_token_pos)
    val root = AbstractSyntaxTree(nodes)
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
        try{
          if (token_pos == tokenizer.tokenList.length){  //if statements are ended
            return (token, token_pos, resultx)
          }
          else{
            throw new Exception() //invalid statement
          }
        }
        catch {case ex:Exception => println("Error: Invalid Statement parser"); exit}
      }
    }
    
    val (cur_tok, cur_tok_pos, new_results) = get_next_statement(cur_token, cur_token_pos, results)
    return (cur_tok, cur_tok_pos, new_results)
  }
  
  //returning single statement node
  def statement(current_token:Token, current_token_pos:Int): (Token, Int, AST) = {
    try{  
      //calling declaration statement
      if (current_token.getType() == TokenType.VAR_TYPE){
        val (tok,pos,node) = delare_statement(current_token, current_token_pos, 0)
        return (tok,pos,node)
      }
      else if (current_token.getType() == TokenType.CONST_TYPE){
        val (tok,pos,node) = delare_statement(current_token, current_token_pos, 1)
        return (tok,pos,node)
      }
      
      //calling skip statement
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
  
      //calling if statement
      else if (current_token.getType() == TokenType.IF){
        val (tok, pos, node) = if_statement(current_token,current_token_pos)
        return (tok, pos, node)
      }
      
      
      else if (current_token.getType() == TokenType.IDENTIFIER){
        if(tokenizer.lookAhead(current_token_pos)._1.getToken().equals("=")){
          val (tok, pos, node) = assign_statement(current_token, current_token_pos)
          return (tok, pos, node)
        } else if((("-|==|><|and|or|\\^|>|<|\\+|\\/|\\*").r.pattern.matcher(tokenizer.lookAhead(current_token_pos)._1.getToken())).find()) {
          val (tok, pos, node) = expr(current_token, current_token_pos)
          return (tok, pos, node)
        } else {
          val (tok, pos, node) = variable(current_token, current_token_pos, 0)
          return (tok, pos, node)
        }
      }
      
      else if (current_token.getType() == TokenType.ALPHA_LITERAL || current_token.getType() == TokenType.BOOL_LITERAL || current_token.getType() == TokenType.INT_LITERAL ){
        if((("-|==|><|and|or|\\^|>|<|\\+|\\/|\\*").r.pattern.matcher(tokenizer.lookAhead(current_token_pos)._1.getToken())).find()){
          val (tok, pos, node) = expr(current_token, current_token_pos)
          return (tok,pos, node)
        } else {
          val (tok, pos, node) = atom(current_token, current_token_pos)
          return (tok, pos, node)
        }
      }
      
      else if (current_token.getType() == TokenType.WHILE){
        val (tok, pos, node) = while_statement(current_token, current_token_pos)
        return (tok, pos, node)
      }
      
      else if (current_token.getType() == TokenType.PRINT){
        val (tok, pos, node) = print_statement(current_token, current_token_pos)
        return (tok, pos, node)
      }
      
      else throw new Exception()
    }
    catch {case ex:Exception => println("Error: Program ended unsuccessfully"); exit}
  }
  
  //returning atomic value
  def atom(current_token: Token, current_token_pos:Int): (Token, Int, AST) = {
    current_token.getType() match {
      case TokenType.UOP => {
        val (cur_token, cur_token_pos) = eat(current_token, TokenType.UOP, current_token_pos)
        val (cur_tok, cur_pos, node) = atom(cur_token, cur_token_pos)
        return (cur_tok, cur_pos, UnaryOp(current_token, node))
      }
      
      case TokenType.INT_LITERAL => {
        val (cur_token, cur_token_pos) = eat(current_token, TokenType.INT_LITERAL, current_token_pos)
        return (cur_token, cur_token_pos, Num(current_token))
      }
      
      case TokenType.BOOL_LITERAL => {
        val (cur_token, cur_token_pos) = eat(current_token, TokenType.BOOL_LITERAL, current_token_pos)
        return (cur_token, cur_token_pos, Bool(current_token))
      }
      
      case TokenType.ALPHA_LITERAL => {
        val (cur_token, cur_token_pos) = eat(current_token, TokenType.ALPHA_LITERAL, current_token_pos)
        return (cur_token, cur_token_pos, Alpha(current_token))
      }
      
      case TokenType.NIL => {
        val (cur_token, cur_token_pos) = eat(current_token, TokenType.NIL, current_token_pos)
        return (cur_token, cur_token_pos, Nil(current_token))
      }
      
      case _ => {
        val node = Var(current_token)
        val (cur_token, cur_token_pos) = eat(current_token, current_token.getType(), current_token_pos)
        return (cur_token, cur_token_pos, node)
      }
    }
  }
  
  //parsing terms (terms having higher priority than expressions) 
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
  
  //parsing expression
  def expr(current_token: Token, current_token_pos:Int): (Token, Int, AST) = {
    val (cur_token, cur_token_pos, left_AST) = term(current_token, current_token_pos)
    def recurse_expr(cur_token: Token, cur_token_pos: Int, left_AST: AST): (Token,Int,AST) = { 
      if (cur_token.getType() == TokenType.BOP){
        try{
          if(cur_token.getToken().equals("==") || cur_token.getToken().equals("<") || 
              cur_token.getToken().equals(">") || cur_token.getToken().equals("><") || 
              cur_token.getToken().equals("and") || cur_token.getToken().equals("or") ||
              cur_token.getToken().equals("^")){
            val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.BOP, cur_token_pos)
            val (tok,pos,right_bin_op) = term(cur_tok, cur_tok_pos)
            val next_node = BinOp(left_AST, cur_token, right_bin_op)
            recurse_expr(tok, pos, next_node)
          } else throw new Exception()
        } catch {case ex:Exception => println("Error: Boolean expression expected"); exit}
      }
      else if (cur_token.getType() == TokenType.PLUS){
        val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.PLUS, cur_token_pos)
        val (tok,pos,right_bin_op) = term(cur_tok, cur_tok_pos)
        val next_node = BinOp(left_AST, cur_token, right_bin_op)
        recurse_expr(tok, pos, next_node)
      }
      
      else if (cur_token.getType() == TokenType.SUB){
        val (cur_tok, cur_tok_pos) = eat(cur_token, TokenType.SUB, cur_token_pos)
        val (tok,pos,right_bin_op) = term(cur_tok, cur_tok_pos)
        val next_node = BinOp(left_AST, cur_token, right_bin_op)
        recurse_expr(tok, pos, next_node)
      }
      
        else (cur_token,cur_token_pos,left_AST)
    }
    val (tok, pos, node) = recurse_expr(cur_token, cur_token_pos, left_AST) 
    return (tok,pos,node)
  }
  
  //parsing print statements
  def print_statement(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
    val (current_tok, current_tok_pos) = eat(current_token, TokenType.PRINT,current_token_pos)
    val (cur_tok, cur_pos, expr_node) = statement(current_tok, current_tok_pos)
    val print_node = PrintStatement(expr_node)
    return (cur_tok, cur_pos, print_node)
  }
  
  //parsing while statements
  def while_statement(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
    val (current_tok, current_tok_pos) = eat(current_token, TokenType.WHILE,current_token_pos)
    val (cur_tok, cur_pos, expr_node) = expr(current_tok, current_tok_pos)
    val (cur_token, cur_token_pos) = eat(cur_tok, TokenType.DO, cur_pos)
    val (tok, pos, statement_node) = statement_list(cur_token, cur_token_pos)
    val while_node = WhileLoop(expr_node, statement_node)
    return (tok, pos, while_node)
  }
  
  //parsing If-then-else statements
  def if_statement(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
      val (current_tok, current_tok_pos) = eat(current_token, TokenType.IF,current_token_pos)
      val (cur_tok, cur_pos, expr_node) = expr(current_tok, current_tok_pos)
      try{
        if (cur_tok.getType() == TokenType.THEN){
          val (cur_token, cur_token_pos) = eat(cur_tok, TokenType.THEN, cur_pos)
          val (tok, pos, statement_node) = statement(cur_token, cur_token_pos)
          try{  
            if (tok.getType() == TokenType.ELSE){
              val (crtk, crtp) = eat(tok, TokenType.ELSE, pos)
              val (tk, tp, second_statement_node) = statement(crtk, crtp)
              val if_node = IfElse(expr_node,statement_node,second_statement_node)
              return (tk, tp, if_node)
            } else throw new Exception()
          } catch {case ex:Exception => println("Error: Expecting \'else\'"); exit}
        } else throw new Exception()
      } catch {case ex:Exception => println("Error: Expecting \'then\'"); exit}
  }
  
  //parsing assignment of already declared variables
  def assign_statement(current_token: Token, current_token_pos: Int): (Token, Int, AST) = {
    val left_node = Var(current_token)
    val (cur_token, cur_token_pos) = eat(current_token, TokenType.IDENTIFIER, current_token_pos)
    val (current_tok, current_tok_pos) = eat(cur_token, TokenType.ASSIGNMENT,cur_token_pos)
    val (tok, pos, right_node) = expr(current_tok, current_tok_pos)
    val next_node = Assign(left_node, current_tok, right_node)
    return (tok, pos, next_node)
  }
  
  //parsing declaration statements
  def delare_statement(current_token: Token, current_token_pos: Int, var_type:Int): (Token, Int, AST) = {
    val (curr_tok, curr_tok_pos) = eat(current_token, current_token.getType(),current_token_pos)
    val (cur_token, cur_token_pos,left_node) = variable(curr_tok, curr_tok_pos,var_type)
    val (current_tok, current_tok_pos) = eat(cur_token, TokenType.COLON,cur_token_pos)
    val data_type_token = current_tok
    val (cur_tok, cur_tok_pos) = eat(current_tok, TokenType.DATA_TYPE,current_tok_pos)
    if(cur_tok.getType() != TokenType.ASSIGNMENT){
      try{
        if (cur_tok.getType() == TokenType.BREAK){
          val next_node = VarDec(left_node, cur_token, Nil(new Token("",TokenType.NIL)))
          return (cur_tok, cur_tok_pos, next_node)
        }
        else throw new Exception()
      } catch {case ex:Exception => println("Error: Invalid Assignment Statement"); exit}
    }
    val (c_tok, tok_pos) = eat(cur_tok, TokenType.ASSIGNMENT,cur_tok_pos)
    if (data_type_token.getToken().equals("int")){
      Try(c_tok.getToken().r.pattern.matcher("-?[0-9]+$").find()).getOrElse(
          try{
            if(c_tok.getType() == TokenType.NIL) Success
            else if(c_tok.getType() == TokenType.IDENTIFIER) Success
            else throw new Exception()
          } catch {case ex:Exception => println("Error: Value should be Integer"); exit}
       )
    } else if (data_type_token.getToken().equals("bool")){
      Try(c_tok.getOriginalToken().toBoolean).getOrElse(
          try{
            if(c_tok.getType() == TokenType.NIL) Success
            else if(c_tok.getType() == TokenType.IDENTIFIER) Success
            else throw new Exception()
          } catch {case ex:Exception => println("Error: Value should be Boolean"); exit}
      )
    } else if (data_type_token.getToken().equals("alpha")){
      if (c_tok.getOriginalToken().charAt(0).!=('\"')){
        try{
          if(c_tok.getType() == TokenType.NIL) Success
          else if(c_tok.getType() == TokenType.IDENTIFIER) Success
          else throw new Exception()
        } catch {case ex:Exception => println("Error: Value should be Alpha"); exit}
      }
    }
    
    val (tok, pos, right_node) = expr(c_tok, tok_pos)
    val next_node = VarDec(left_node, cur_token, right_node)
    return (tok, pos, next_node)
  }
  
  //defining variable node
  def variable(current_token: Token, current_token_pos: Int, var_type:Int): (Token, Int, AST) = {
    try{
      if (var_type==0){
        val node = Var(current_token)
        val (cur_token, cur_token_pos) = eat(current_token, TokenType.IDENTIFIER, current_token_pos)
        return (cur_token, cur_token_pos, node)
      }
      else if (var_type==1){ 
        val node = Const(current_token)
        val (cur_token, cur_token_pos) = eat(current_token, TokenType.IDENTIFIER, current_token_pos)
        return (cur_token, cur_token_pos, node)
      }
      else throw new Exception()
    } catch {case ex:Exception => println("Error: Invalid Variable type"); exit}
  }
  
  def parse(): AST = {
    val (tok,pos,tree) = compound_statement(cr_token, cr_token_pos) 
    //println(tree)
    return tree
  }
}