package token;
import scala.util.matching.Regex

class TokenData(val patternx: Regex, val Typex: TokenType.EnumVal) {
  private val pattern = patternx;
  private val Type: TokenType.EnumVal = Typex;
  
  def getPattern(): Regex = pattern
  def getType(): TokenType.EnumVal = Type
}