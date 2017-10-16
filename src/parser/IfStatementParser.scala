package parser

class IfStatementParser(override val codeLine: String) extends Parser(codeLine){
  override def isParsable(): Boolean = {
    val identifier: String = "([a-zA-Z]+[A-Za-z0-9_$*#]*)"
    val value: String = "(((-?)[0-9]+)|(([t]{2})|([f]{2}))|(([\"|\']).*([\"|\'])$))"
    val expr: String = identifier+"|"+value
    val cond: String = "((t){2}|(f){2})"
    val reg: String = "^(if) "+cond+" then "+expr+" else "+expr
    //println(reg)
    if(!(reg.r findAllIn line).mkString(",").equals("")) return true
    else return false
  }
  
  override def parse(){
    
  }
}