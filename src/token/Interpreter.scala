package token

import parser._
import parser.ExprParser

case class value(var_type: String, var_name: String, data_type: String, var_value: Any)

class Interpreter(val parser: ExprParser, val var_table: Map[String, value]){
  def visit(node: AST): (Int,Map[String,value]) = {
    if (node.isInstanceOf[BinOp]) visit_BinOp(node.asInstanceOf[BinOp], var_table)
    else if (node.isInstanceOf[UnaryOp]) visit_UnaryOp(node.asInstanceOf[UnaryOp], var_table)
    else if (node.isInstanceOf[Num]) visit_Num(node.asInstanceOf[Num], var_table)
    else if (node.isInstanceOf[Bool]) visit_Bool(node.asInstanceOf[Bool], var_table)
    else if (node.isInstanceOf[Alpha]) visit_Alpha(node.asInstanceOf[Alpha], var_table)
    else if (node.isInstanceOf[PrintStatement]) visit_Print(node.asInstanceOf[PrintStatement], var_table)
    else if (node.isInstanceOf[IfElse]) visit_If(node.asInstanceOf[IfElse], var_table)
    else if (node.isInstanceOf[WhileLoop]) visit_While(node.asInstanceOf[WhileLoop], var_table)
    else if (node.isInstanceOf[Assign]) visit_Assign(node.asInstanceOf[Assign], var_table)
    else if (node.isInstanceOf[Var]) visit_Var(node.asInstanceOf[Var], var_table)
    else if (node.isInstanceOf[VarDec]) visit_VarDec(node.asInstanceOf[VarDec], var_table)
    else throw new Exception("Invalid node")
  }
  
  def visit_Compound(node: Compound, var_table: Map[String, value]) = {
    for(child <- node.children){
      visit(child.asInstanceOf[AST])
    }
  }
  
  def visit_VarDec(node: VarDec, var_table: Map[String, value]): (Int, Map[String,value]) = {
    val var_name = node.left.asInstanceOf[Var].token.getToken()
    val var_value = var_table(var_name)
    if (!var_value.var_name.equals("null")){
      throw new Exception("Variable is already declared")
    }
    val right_node = visit(node.right)
    val new_var_table = var_table.+(var_name -> (new value("var", var_name, "int" ,right_node)))
    return (1, new_var_table)
  }
  
  def visit_Assign(node: Assign, var_table: Map[String,value]): (Int,Map[String,value]) = {
    val var_name = node.left.asInstanceOf[Var].token.getToken()
    val var_value = var_table(var_name)
    if (var_value.var_name.equals("null")){
      throw new Exception("Variable not declared")
    }
    val right_node = visit(node.right)
    val new_var_table = var_table.+(var_name -> (new value(var_value.var_type, var_value.var_name, var_value.data_type, right_node)))
    return (1, new_var_table)
  }
  
  def visit_Var(node: Var, var_table: Map[String, value]): (Int, Map[String,value]) = {
    
    return (0, var_table)
  }
  
  def visit_Print(node: PrintStatement, var_table: Map[String, value]):(Int, Map[String,value]) = {
    return (0, var_table)
  }
  
  def visit_While(node: WhileLoop, var_table: Map[String, value]):(Int, Map[String,value]) = {
    return (0, var_table)
  }
  
  def visit_If(node: IfElse, var_table: Map[String, value]):(Int, Map[String,value]) = {
    return (0, var_table)
  }

  def visit_UnaryOp(node: UnaryOp, var_table: Map[String, value]): (Int, Map[String,value]) = {
    if (node.token.getType() == TokenType.UOP) return (-visit(node.expr_node)._1,var_table)
    else return (0,var_table)
  }
  
  def visit_BinOp(node: BinOp, var_table: Map[String, value]): (Int, Map[String,value]) = {
    if ((node.right.getClass() == node.left.getClass()) && !(node.left.getClass().toString.equals("class parser.Alpha"))){
      if(node.token.getType()==TokenType.PLUS) return (((visit(node.left)._1) + (visit(node.right)._1)), var_table)
      else if (node.token.getType()==TokenType.MUL) return (((visit(node.left)._1) * (visit(node.right)._1)), var_table)
      else if (node.token.getType()==TokenType.DIV) return ((visit(node.left)._1 / visit(node.right)._1), var_table)
      else if (node.token.getType()==TokenType.BOP){
        node.token.getToken() match {
           case "^" => (Math.pow(visit(node.left)._1, visit(node.right)._1).toInt, var_table)
           case "==" => if (visit(node.left)._1 == visit(node.right)._1) return (1,var_table) else return (0,var_table) 
           case ">" => if (visit(node.left)._1 > visit(node.right)._1) return (1,var_table) else return (0,var_table)
           case "<" => if (visit(node.left)._1 < visit(node.right)._1) return (1,var_table) else return (0,var_table)
           case "and" => if ((visit(node.left)._1 * visit(node.right)._1) == 0) return (0,var_table) else return (1,var_table)
           case "or" => if ((visit(node.left)._1 + visit(node.right)._1) == 0) return (0,var_table) else return (1,var_table)
         }
      }
      else throw new Exception("Invalid operand")
    } else throw new Exception("Binary operation can not be applied at Alpha")
  }
  
  def visit_Num(node: Num, var_table: Map[String, value]):(Int, Map[String,value]) = return (node.token.getToken().toInt, var_table)
  
  def visit_Bool(node: Bool, var_table: Map[String, value]): (Int, Map[String,value]) = if(node.token.getOriginalToken().toBoolean) return (1,var_table) else return (0,var_table)
  
  def visit_Alpha(node: Alpha, var_table: Map[String, value]): (Int, Map[String,value]) = return (0, var_table) //String = node.token.getOriginalToken()
  
  def interpret(): Any = {
    val tree = parser.parse()
    return visit_Compound(tree.asInstanceOf[Compound], var_table)
  }
}