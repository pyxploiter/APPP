package token

//Enumeration for types of Tokens
object TokenType {
  sealed trait Type
  case object IDENTIFIER extends Type
  case object INTEGER extends Type
  case object BOOLEAN extends Type
  case object ALPHA extends Type
  case object STRING extends Type
  case object ASSIGNMENT extends Type
  case object VARIABLE_TYPE extends Type
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
  
  val typeoftokens = Seq(IDENTIFIER, INTEGER, BOOLEAN, ALPHA, STRING, 
                        EMPTY, SPACE, ASSIGNMENT, VARIABLE_TYPE, 
                        COLON, BOP, DATA_TYPE, RESERVED, BREAK, WHILE, DO, IF, THEN, ELSE,
                        NIL, PRINT, PLUS, MUL, DIV)
}