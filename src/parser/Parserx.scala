package parser
import scala.util.matching.Regex

abstract class Parserx(val codeLine: String) {
  val line: String = codeLine.trim()
  
  def isParsable: Boolean
  def parse
}