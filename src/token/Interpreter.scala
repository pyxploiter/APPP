package token

class Interpreter(val parser: Parser){
  def visit(node: AST): Int = {
    if (node.isInstanceOf[BinOp]) visit_BinOp(node.asInstanceOf[BinOp])
    else if (node.isInstanceOf[Num]) visit_Num(node.asInstanceOf[Num])
    else if (node.isInstanceOf[UnaryOp]) visit_UnaryOp(node.asInstanceOf[UnaryOp])
    else throw new Exception("Invalid node")
  }
  
  def visit_UnaryOp(node: UnaryOp): Int = {
    if (node.token.getType() == TokenType.UOP) return -visit(node.expr_node)
    else return 0
  }
  
  def visit_BinOp(node: BinOp): Int = {
   if(node.token.getType()==TokenType.PLUS) return (visit(node.left) + visit(node.right))
   else if (node.token.getType()==TokenType.MUL) return (visit(node.left) * visit(node.right))
   else if (node.token.getType()==TokenType.DIV) return (visit(node.left) / visit(node.right))
   else return 0
  }
  
  def visit_Num(node: Num): Int = {
    node.token.getToken().toInt
  }
  
  def interpret(): Int = {
    val tree = parser.parse()
    return visit(tree.asInstanceOf[AST])
  }
}