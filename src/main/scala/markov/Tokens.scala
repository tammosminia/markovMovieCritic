package markov

object Tokens {
  abstract class Token
  case class WordToken(word: String) extends Token
  object StartToken extends Token
  object EndSentence extends Token
  object EndToken extends Token

  private def dropDoubleEndSentences(tokens: List[Token]): List[Token] = tokens match {
    case Nil => Nil
    case EndSentence :: EndSentence :: tail => dropDoubleEndSentences(EndSentence :: tail)
    case head :: tail => head :: dropDoubleEndSentences(tail)
  }

  def tokenize(plot: String): List[Token] = {
    val endWithDot = plot + "."
    val dropSigns = endWithDot.replaceAll("""[:]""", "")
    val separateDots = dropSigns.replaceAll("""[,;!.]""", " . ")
    val words = separateDots.toLowerCase.split("""\s+""").toList
    val tokens = words.flatMap {
      case "" => None
      case "." => Some(EndSentence)
      case word => Some(WordToken(word))
    }
    StartToken :: dropDoubleEndSentences(tokens).:+(EndToken)
  }

  def tokensToString(tokens: List[Token]): String = {
    val noMargins = tokens.dropWhile(_ == StartToken).takeWhile(_ != EndToken)
    def r(tokens: List[Token], capitalize: Boolean, addSpace: Boolean): String = tokens match {
      case Nil => ""
      case WordToken(word) :: tail =>
        val prefix = if (addSpace) " " else ""
        val properCapitalization = if (capitalize) word.capitalize else word
        prefix + properCapitalization + r(tail, false, true)
      case EndSentence :: tail => "." + r(tail, true, true)
    }
    r(noMargins, true, false)
  }

}
