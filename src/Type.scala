//Enumeration for types of Tokens
object Type {
  sealed trait EnumVal
  case object INTEGER extends EnumVal
  case object BOOLEAN extends EnumVal
  case object ALPHA extends EnumVal
  val typeoftokens = Seq(INTEGER, BOOLEAN, ALPHA)
}