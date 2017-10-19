package token

//Enumeration for types of Tokens
object TokenType {
  sealed trait Type
  case object IDENTIFIER extends Type
  case object INT_LITERAL extends Type
  case object BOOL_LITERAL extends Type
  case object ALPHA_LITERAL extends Type
  case object STRING extends Type
  case object ASSIGNMENT extends Type
  case object VAR_TYPE extends Type
  case object CONST_TYPE extends Type
  case object COLON extends Type
  case object EMPTY extends Type
  case object SPACE extends Type
  case object BOP extends Type
  case object UOP extends Type
  case object DATA_TYPE extends Type
  case object RESERVED extends Type
  case object BREAK extends Type
  case object WHILE extends Type
  case object DO extends Type
  case object IF extends Type
  case object THEN extends Type
  case object ELSE extends Type
  case object NIL extends Type
  case object PRINT extends Type
  case object PLUS extends Type
  case object MUL extends Type
  case object DIV extends Type
  case object SKIP extends Type
}