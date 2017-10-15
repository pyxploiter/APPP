package token

//Enumeration for types of Tokens
object TokenType {
  sealed trait EnumVal
  case object IDENTIFIER extends EnumVal
  case object INTEGER extends EnumVal
  case object BOOLEAN extends EnumVal
  case object ALPHA extends EnumVal
  case object STRING extends EnumVal
  case object SEMICOLON extends EnumVal
  case object ASSIGNMENT extends EnumVal
  case object VARIABLE_TYPE extends EnumVal
  case object COLON extends EnumVal
  case object EMPTY extends EnumVal
  case object SPACE extends EnumVal
  case object BOP extends EnumVal
  case object UOP extends EnumVal
  case object DATA_TYPE extends EnumVal
  case object RESERVED extends EnumVal
  val typeoftokens = Seq(IDENTIFIER, INTEGER, BOOLEAN, ALPHA, STRING, 
                        EMPTY, SPACE, SEMICOLON, ASSIGNMENT, VARIABLE_TYPE, COLON, BOP, DATA_TYPE, RESERVED)
}