package parser
import scala.util.matching.Regex

class PrintParser(override val codeLine: String) extends Parserx(codeLine){
  override def isParsable(): Boolean = {
    if(!("^(print) ([\"|\']).*([\"|\'])$".r findAllIn line).mkString(",").equals("")) return true
    else return false
  }
  
  override def parse(){
    val matcher = "print \"".r.pattern.matcher(codeLine)
    if (matcher.find()){
      println(codeLine.substring(matcher.end(),codeLine.length()-1))
    }
  }
}