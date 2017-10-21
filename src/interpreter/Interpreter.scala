package interpreter

import parser._
import parser.Parser
import token.TokenType

case class value(var_type: String, var_name: String, data_type: String, var_value: Int)

class Interpreter(val parser: Parser, val var_table: Map[String, value]){
  //visiting each node of ASR
  def visit(node: AST, var_table: Map[String, value]): (Int,Map[String,value]) = {
    try{
      if (node.isInstanceOf[BinOp]) evaluate_BinOp(node.asInstanceOf[BinOp], var_table)
      else if (node.isInstanceOf[UnaryOp]) evaluate_UnaryOp(node.asInstanceOf[UnaryOp], var_table)
      else if (node.isInstanceOf[Num]) evaluate_Num(node.asInstanceOf[Num], var_table)
      else if (node.isInstanceOf[Bool]) evaluate_Bool(node.asInstanceOf[Bool], var_table)
      else if (node.isInstanceOf[Alpha]) evaluate_Alpha(node.asInstanceOf[Alpha], var_table)
      else if (node.isInstanceOf[VarDec]) evaluate_VarDec(node.asInstanceOf[VarDec], var_table)
      else if (node.isInstanceOf[Assign]) evaluate_Assign(node.asInstanceOf[Assign], var_table)
      else if (node.isInstanceOf[Var]) evaluate_Var(node.asInstanceOf[Var], var_table)
      else if (node.isInstanceOf[IfElse]) evaluate_If(node.asInstanceOf[IfElse], var_table)
      else if (node.isInstanceOf[WhileLoop]) evaluate_While(node.asInstanceOf[WhileLoop], var_table)
      else if (node.isInstanceOf[PrintStatement]) evaluate_Print(node.asInstanceOf[PrintStatement], var_table)
      else if (node.isInstanceOf[Nil]) (0,var_table)
      else throw new Exception()
    } catch {case ex:Exception => println("Error: Invalid Statement"); exit}
  }
  
  // evaluating complete Abstract syntax tree
  def evaluate_AST(node: AbstractSyntaxTree, var_table: Map[String, value]) = {
    def evaluate_nodes(node: List[AST], var_table: Map[String, value]){
      //println(node)
      if(!node.isEmpty){
        val new_var_table = visit(node.head, var_table)._2
        evaluate_nodes(node.tail, new_var_table)
      }
    }
    val new_table = visit(node.children.head.asInstanceOf[AST], var_table)._2
    evaluate_nodes(node.children.tail, new_table)
  }
  
  // evaluating variable declaration node
  def evaluate_VarDec(node: VarDec, var_table: Map[String, value]): (Int, Map[String,value]) = {
    val var_name = if (node.left.isInstanceOf[Var]) node.left.asInstanceOf[Var].token.getToken() else node.left.asInstanceOf[Const].token.getToken() 
    val var_value = var_table(var_name)
    if (!var_value.var_name.equals("null")) throw new Exception("Variable \""+var_name+"\" is already declared")
    val right_node = visit(node.right, var_table)
    val new_value = if(node.left.isInstanceOf[Var]) new value("var", var_name, "int" ,right_node._1) else new value("const", var_name, "int" ,right_node._1)
    val new_var_table = var_table+(var_name -> new_value)
    return (1, new_var_table)
  }
  
  // evaluating assignment node
  def evaluate_Assign(node: Assign, var_table: Map[String,value]): (Int,Map[String,value]) = {
    try{
    if((var_table.apply(node.left.asInstanceOf[Var].token.getToken()).var_type).equals("var")){
      val var_name = node.left.asInstanceOf[Var].token.getToken()
      val var_value = var_table(var_name)
      if (var_value.var_name.equals("null")){
        throw new Exception("Error: Variable not declared")
      }
      val right_node = visit(node.right, var_table)._1
      val new_var_table = var_table.+(var_name -> (new value(var_value.var_type, var_value.var_name, var_value.data_type, right_node)))
      return (1, new_var_table)
    }
    else throw new Exception()
    } catch {case ex:Exception => println("Error: Reassigning 'const' is not allowed"); exit}
  }
  
  // getting variable value from Var node
  def evaluate_Var(node: Var, var_table: Map[String, value]): (Int, Map[String,value]) = {
    try{
      if (!var_table(node.token.getToken()).var_name.equals("null")){
        val int_value = var_table(node.token.getToken()).var_value
        return (int_value, var_table)
      }
      else throw new Exception()
    } catch { case ex: Exception => println("Error: variable \""+node.token.getToken()+"\" not found"); exit } 
    return (0, var_table)
  }
  
  // evaluating print node
  def evaluate_Print(node: PrintStatement, var_table: Map[String, value]):(Int, Map[String,value]) = {
    val answer = visit(node.statement, var_table)._1
    if(!node.statement.isInstanceOf[Alpha])
      println(answer)
    return (1, var_table)
  }
  
  // evaluating while-loop node
  def evaluate_While(node: WhileLoop, var_table: Map[String, value]):(Int, Map[String,value]) = {
    def recurse_do(ans:Int, var_table : Map[String, value] ): (Int, Map[String, value])={
      if (visit(node.while_cond,var_table)._1 != 0){
        def recurse_do_statements(ans:Int, var_table: Map[String,value], statements: List[AST]): (Int, Map[String, value]) = {
          if (!statements.isEmpty){
            val (answer, new_table) = visit(statements.head, var_table)
            recurse_do_statements(answer, new_table, statements.tail)
          } else {
            return (ans, var_table) 
          }
        }
        val (answer, new_table) = recurse_do_statements(ans, var_table, node.do_statement)
        recurse_do(answer,new_table)         
      }
      else return (ans,var_table) 
    }
    val (final_ans,final_table)= recurse_do(0,var_table)
    return (final_ans,final_table)
  }
  
  // evaluating if-else node
  def evaluate_If(node: IfElse, var_table: Map[String, value]):(Int, Map[String,value]) = {
    if(visit(node.if_node, var_table)._1 != 0){
      val answer = visit(node.then_node, var_table)._1
      return (answer, var_table)
    } else {
      val answer = visit(node.else_node, var_table)._1
      return (answer, var_table)
    }
  }
  
  // evaluating unary operation
  def evaluate_UnaryOp(node: UnaryOp, var_table: Map[String, value]): (Int, Map[String,value]) = {
    try{
      if (node.token.getType() == TokenType.UOP){
        if (node.expr_node.isInstanceOf[Bool]){
          if (visit(node.expr_node, var_table)._1 != 0){
            return (0,var_table)
          } else return (1, var_table)
        }
        else return (-visit(node.expr_node, var_table)._1,var_table)
      }
      else throw new Exception()
   } catch { case ex: Exception => println("Error: Invalid operation. Only '~' and 'not' unary operators are allowed"); exit }
   return (0,var_table)
  }
  
  // evaluating binary operations
  def evaluate_BinOp(node: BinOp, var_table: Map[String, value]): (Int, Map[String,value]) = {
    try{  
      if (!node.left.getClass().toString.equals("class parser.Alpha") && !node.right.getClass().toString.equals("class parser.Alpha")){
        if(node.token.getType()==TokenType.PLUS) return (((visit(node.left,var_table)._1) + (visit(node.right,var_table)._1)), var_table)
        else if (node.token.getType()==TokenType.MUL) return (((visit(node.left,var_table)._1) * (visit(node.right,var_table)._1)), var_table)
        else if (node.token.getType()==TokenType.DIV) return ((visit(node.left,var_table)._1 / visit(node.right,var_table)._1), var_table)
        else if (node.token.getType()==TokenType.SUB) return ((visit(node.left,var_table)._1 - visit(node.right,var_table)._1), var_table)
        else if (node.token.getType()==TokenType.BOP){
          node.token.getToken() match {
             case "^" => return (Math.pow(visit(node.left,var_table)._1, visit(node.right,var_table)._1).toInt, var_table);
             case "==" => if (visit(node.left,var_table)._1 == visit(node.right,var_table)._1) return (1,var_table) else return (0,var_table) 
             case "><" => if (visit(node.left,var_table)._1 != visit(node.right,var_table)._1) return (1,var_table) else return (0,var_table) 
             case ">" => if (visit(node.left,var_table)._1 > visit(node.right,var_table)._1) return (1,var_table) else return (0,var_table)
             case "<" => if (visit(node.left,var_table)._1 < visit(node.right,var_table)._1) return (1,var_table) else return (0,var_table)
             case "and" => if ((visit(node.left,var_table)._1 * visit(node.right,var_table)._1) == 0) return (0,var_table) else return (1,var_table)
             case "or" => if ((visit(node.left,var_table)._1 + visit(node.right,var_table)._1) == 0) return (0,var_table) else return (1,var_table)
           }
        }
      }
      else throw new Exception()
    } catch { case ex:Exception => println("Error: Invalid operands in binary operation"); exit }
    return (0, var_table) //never returned
  }
  
  // evaluating integer node
  def evaluate_Num(node: Num, var_table: Map[String, value]):(Int, Map[String,value]) = return (node.token.getToken().toInt, var_table)
  
  // evaluating boolean node
  def evaluate_Bool(node: Bool, var_table: Map[String, value]): (Int, Map[String,value]) = if(node.token.getOriginalToken().toBoolean) return (1,var_table) else return (0,var_table)
  
  // evaluating Alpha node
  def evaluate_Alpha(node: Alpha, var_table: Map[String, value]): (Int, Map[String,value]) = {
    print(node.token.getToken().substring(1, node.token.getToken().length()-1))
    return (1, var_table) 
  }
  
  // interpreting the tree
  def interpret(): Boolean = {
    val tree = parser.parse()
    evaluate_AST(tree.asInstanceOf[AbstractSyntaxTree], var_table)
    return true
  }
}