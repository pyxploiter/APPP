package interpreter

import parser._
import parser.ExprParser
import token.TokenType

case class value(var_type: String, var_name: String, data_type: String, var_value: Int)

class Interpreter(val parser: ExprParser, val var_table: Map[String, value]){
  def visit(node: AST, var_table: Map[String, value]): (Int,Map[String,value]) = {
    println(node.getClass)
    if (node.isInstanceOf[BinOp]) evaluate_BinOp(node.asInstanceOf[BinOp], var_table)
    else if (node.isInstanceOf[UnaryOp]) evaluate_UnaryOp(node.asInstanceOf[UnaryOp], var_table)
    else if (node.isInstanceOf[Num]) evaluate_Num(node.asInstanceOf[Num], var_table)
    else if (node.isInstanceOf[Bool]) evaluate_Bool(node.asInstanceOf[Bool], var_table)
    else if (node.isInstanceOf[Alpha]) evaluate_Alpha(node.asInstanceOf[Alpha], var_table)
    else if (node.isInstanceOf[PrintStatement]) evaluate_Print(node.asInstanceOf[PrintStatement], var_table)
    else if (node.isInstanceOf[IfElse]) evaluate_If(node.asInstanceOf[IfElse], var_table)
    else if (node.isInstanceOf[WhileLoop]) evaluate_While(node.asInstanceOf[WhileLoop], var_table)
    else if (node.isInstanceOf[Assign]) evaluate_Assign(node.asInstanceOf[Assign], var_table)
    else if (node.isInstanceOf[Var]) evaluate_Var(node.asInstanceOf[Var], var_table)
    else if (node.isInstanceOf[VarDec]) evaluate_VarDec(node.asInstanceOf[VarDec], var_table)
    else throw new Exception("Invalid node")
  }
  
  // giving evaluation commands to each statment one by one
  def evaluate_Compound(node: Compound, var_table: Map[String, value]) = {
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
    val var_name = node.left.asInstanceOf[Var].token.getToken()
    val var_value = var_table(var_name)
    if (!var_value.var_name.equals("null")){
      throw new Exception("Variable \""+var_name+"\" is already declared")
    }
    val right_node = visit(node.right, var_table)
    val new_var_table = var_table+(var_name -> (new value("var", var_name, "int" ,right_node._1)))
    return (1, new_var_table)
  }
  
  // evaluating assignment node
  def evaluate_Assign(node: Assign, var_table: Map[String,value]): (Int,Map[String,value]) = {
    val var_name = node.left.asInstanceOf[Var].token.getToken()
    val var_value = var_table(var_name)
    if (var_value.var_name.equals("null")){
      throw new Exception("Error: Variable not declared")
    }
    val right_node = visit(node.right, var_table)._1
    val new_var_table = var_table.+(var_name -> (new value(var_value.var_type, var_value.var_name, var_value.data_type, right_node)))
    return (1, new_var_table)
  }
  
  // getting variable value from Var node
  def evaluate_Var(node: Var, var_table: Map[String, value]): (Int, Map[String,value]) = {
    if (!var_table(node.token.getToken()).var_name.equals("null")){
      val int_value = var_table(node.token.getToken()).var_value
      return (int_value, var_table)
    } else throw new Exception("Error: Variable \""+node.token.getToken()+"\" is not declared")
  }
  
  // evaluating print node
  def evaluate_Print(node: PrintStatement, var_table: Map[String, value]):(Int, Map[String,value]) = {
    val answer = visit(node.statement, var_table)._1
    println(answer)
    return (1, var_table)
  }
  
  // evaluating while-loop node
//  def evaluate_While(node: WhileLoop, var_table: Map[String, value]):(Int, Map[String,value]) = {
//    if (visit(node.while_cond,var_table)._1 != 0){
//      val answer = visit(node.do_statement, var_table)._1
//      return (answer,var_table)
//    }
//    return (0, var_table)
//  }
  
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
    if (node.token.getType() == TokenType.UOP){
      if (node.expr_node.isInstanceOf[Bool]){
        if (visit(node.expr_node, var_table)._1 != 0){
          return (0,var_table)
        } else return (1, var_table)
      }
      else return (-visit(node.expr_node, var_table)._1,var_table)
    }
    else return (0,var_table)
  }
  
  // problem here : 3+2 == 2 ==> binOp == Num will cause invalid binary operation
  def evaluate_BinOp(node: BinOp, var_table: Map[String, value]): (Int, Map[String,value]) = {
    if ((node.right.getClass() == node.left.getClass()) && !(node.left.getClass().toString.equals("class parser.Alpha"))){
      if(node.token.getType()==TokenType.PLUS) return (((visit(node.left,var_table)._1) + (visit(node.right,var_table)._1)), var_table)
      else if (node.token.getType()==TokenType.MUL) return (((visit(node.left,var_table)._1) * (visit(node.right,var_table)._1)), var_table)
      else if (node.token.getType()==TokenType.DIV) return ((visit(node.left,var_table)._1 / visit(node.right,var_table)._1), var_table)
      else if (node.token.getType()==TokenType.BOP){
        node.token.getToken() match {
           case "^" => (Math.pow(visit(node.left,var_table)._1, visit(node.right,var_table)._1).toInt, var_table)
           case "==" => if (visit(node.left,var_table)._1 == visit(node.right,var_table)._1) return (1,var_table) else return (0,var_table) 
           case ">" => if (visit(node.left,var_table)._1 > visit(node.right,var_table)._1) return (1,var_table) else return (0,var_table)
           case "<" => if (visit(node.left,var_table)._1 < visit(node.right,var_table)._1) return (1,var_table) else return (0,var_table)
           case "and" => if ((visit(node.left,var_table)._1 * visit(node.right,var_table)._1) == 0) return (0,var_table) else return (1,var_table)
           case "or" => if ((visit(node.left,var_table)._1 + visit(node.right,var_table)._1) == 0) return (0,var_table) else return (1,var_table)
         }
      }
      else throw new Exception("Invalid operand")
    } else throw new Exception("Invalid Binary Operation")
  }
  
  def evaluate_Num(node: Num, var_table: Map[String, value]):(Int, Map[String,value]) = return (node.token.getToken().toInt, var_table)
  
  def evaluate_Bool(node: Bool, var_table: Map[String, value]): (Int, Map[String,value]) = if(node.token.getOriginalToken().toBoolean) return (1,var_table) else return (0,var_table)
  
  def evaluate_Alpha(node: Alpha, var_table: Map[String, value]): (Int, Map[String,value]) = return (0, var_table) //String = node.token.getOriginalToken()
  
  def interpret(): Any = {
    val tree = parser.parse()
    return evaluate_Compound(tree.asInstanceOf[Compound], var_table)
  }
}