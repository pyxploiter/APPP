package parser

import token.Token
import token.TokenType
import token.Tokenizer
import token.TokenType

trait AST

case class BinOp(val left: AST, val token: Token, val right: AST) extends AST
case class UnaryOp(val token: Token, expr_node: AST) extends AST
case class Num(val token:Token) extends AST
case class Bool(val token:Token) extends AST
case class Alpha(val token:Token) extends AST
case class VarDec(val left:AST, val token: Token, val right: AST) extends AST
case class Assign(val left:AST, val token: Token, val right: AST) extends AST
case class Var(val token: Token) extends AST
case class Nil(val token:Token, val token_type:TokenType.Type) extends AST
case class Const(val token: Token) extends AST
case class AbstractSyntaxTree(val children: List[AST]) extends AST
case class IfElse(if_node: AST, then_node: AST, else_node: AST) extends AST
case class WhileLoop(while_cond:AST, do_statement:List[AST]) extends AST
case class PrintStatement(statement:AST) extends AST