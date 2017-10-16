package token;
import scala.util.matching.Regex

class TokenData(val patternx: Regex, val Typex: TokenType.Type) {
  private val pattern = patternx;
  private val Type: TokenType.Type = Typex;
  
  def getPattern(): Regex = pattern
  def getType(): TokenType.Type = Type
}