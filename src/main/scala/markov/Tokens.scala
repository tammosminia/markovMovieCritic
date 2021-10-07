package markov

object Tokens {
  sealed abstract class Token
  case class WordToken(word: String) extends Token
  object InfrequentWord extends Token
  case class SignToken(sign: String) extends Token
  object StartToken extends Token
  object EndToken extends Token

  val tokenSignsRegex: String = """([(),;!?.])""" //These become tokens
  val ignoredSignsRegex: String = """[:"&]""" //We drop these

  case class Dictionary(words: Set[String]) {
    def isFrequentWord(w: String): Boolean = words.contains(w)
  }

  object Dictionary {
    def build(plots: List[String], minCount: Int): Dictionary = {
      val allWords = plots.flatMap {
        _.replaceAll(ignoredSignsRegex, " ")
          .replaceAll(tokenSignsRegex, " ")
          .toLowerCase
          .split("""\s+""")
          .toList
      }
      val frequentWords: Set[String] = allWords
        .groupBy(identity)
        .toList
        .collect {
          case (w, l) if l.length >= minCount => w
        }
        .toSet
      Dictionary(frequentWords)
    }

  }

  def tokenize(plot: String, d: Dictionary): List[Token] = {
    val dropSigns = plot.replaceAll(ignoredSignsRegex, " ")
    val separateSigns = dropSigns.replaceAll(tokenSignsRegex, " $0 ")
    val words = separateSigns.toLowerCase.split("""\s+""").toList
    val pattern = tokenSignsRegex.r
    val tokens = words.filterNot(_.isEmpty).collect {
      case pattern(sign)                  => SignToken(sign)
      case word if d.isFrequentWord(word) => WordToken(word)
      case _                              => InfrequentWord
    }
    StartToken :: tokens.:+(EndToken)
  }

  def tokensToString(tokens: List[Token]): String = {
    val noMargins = tokens.dropWhile(_ == StartToken).takeWhile(_ != EndToken)
    def r(tokens: List[Token], capitalize: Boolean, addSpace: Boolean): String = tokens match {
      case Nil => ""
      case WordToken(word) :: tail =>
        val prefix = if (addSpace) " " else ""
        val properCapitalization = if (capitalize) word.capitalize else word
        prefix + properCapitalization + r(tail, false, true)
      case SignToken(s) :: tail => s + r(tail, true, true) //TODO: not all signs have this capitalization and space
    }
    r(noMargins, true, false)
  }

}
