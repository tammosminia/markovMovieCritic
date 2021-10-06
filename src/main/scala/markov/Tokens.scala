package markov

object Tokens {
  sealed abstract class Token
  case class WordToken(word: String) extends Token
  object InfrequentWord extends Token
  object StartToken extends Token
  object EndSentence extends Token
  object EndToken extends Token

  val ignoredSignsRegex: String = """[:()"&]"""
  val endSentenceSignsRegex: String = """[,;!?.]"""

  case class Dictionary(words: Set[String]) {
    def isFrequentWord(w: String): Boolean = words.contains(w)
  }

  object Dictionary {
    def build(plots: List[String]): Dictionary = {
      val allWords = plots.flatMap {
        _.replaceAll(ignoredSignsRegex, " ")
          .replaceAll(endSentenceSignsRegex, " ")
          .toLowerCase
          .split("""\s+""")
          .toList
      }
      val frequentWords: Set[String] = allWords
        .groupBy(identity)
        .toList
        .collect {
          case (w, l) if l.length > 4 => w
        }
        .toSet
      Dictionary(frequentWords)
    }

  }

  private def dropDoubleEndSentences(tokens: List[Token]): List[Token] = tokens match {
    case Nil                                => Nil
    case EndSentence :: EndSentence :: tail => dropDoubleEndSentences(EndSentence :: tail)
    case head :: tail                       => head :: dropDoubleEndSentences(tail)
  }

  def tokenize(plot: String, d: Dictionary): List[Token] = {
    val endWithDot = plot + "."
    val dropSigns = endWithDot.replaceAll(ignoredSignsRegex, " ")
    val separateDots = dropSigns.replaceAll(endSentenceSignsRegex, " . ")
    val words = separateDots.toLowerCase.split("""\s+""").toList
    val tokens = words.filterNot(_.isEmpty).collect {
      case "."                            => EndSentence
      case word if d.isFrequentWord(word) => WordToken(word)
      case _                              => InfrequentWord
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
