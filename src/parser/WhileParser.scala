package parser

class WhileParser(override val codeLine: String) extends Parserx(codeLine){
  override def isParsable(): Boolean = {
    if(!("^(print) ([\"|\']).*([\"|\'])$".r findAllIn line).mkString(",").equals("")) return true
    else return false
  }
  
  override def parse(){
    
  }
}