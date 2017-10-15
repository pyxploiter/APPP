package parser
import scala.util.matching.Regex

abstract class Parser(val codeLine: String) {
  val line: String = codeLine.trim()
  
  def isParsable: Boolean
  def parse
}