package parser

class SkipParser(override val codeLine: String) extends Parser(codeLine){
  override def isParsable(): Boolean = {
    if(!("^(skip)$".r findAllIn line).mkString(",").equals("")) return true
    else return false
  }
  
  override def parse(){
    
  }
}