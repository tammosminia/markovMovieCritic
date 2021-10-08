package markov

import scala.util.matching.Regex

sealed abstract class Token

object Token {
  case class WordToken(word: String) extends Token
  case object InfrequentWord extends Token
  case class SignToken(sign: String) extends Token
  case object StartToken extends Token
  case object EndToken extends Token

  val tokenSignsRegex: Regex = """([(),;!?.])""".r //These become tokens
  val ignoredSignsRegex: Regex = """[:"&]""".r //We drop these

  def tokenize(plot: String, d: Dictionary): Plot = {
    val dropSigns = ignoredSignsRegex.replaceAllIn(plot, " ")
    val separateSigns = tokenSignsRegex.replaceAllIn(dropSigns, " $0 ")
    val split = separateSigns.toLowerCase.split("""\s+""").toList.filterNot(_.isEmpty)
    val tokens = split.collect {
      case tokenSignsRegex(sign)          => SignToken(sign)
      case word if d.isFrequentWord(word) => WordToken(word)
      case _                              => InfrequentWord
    }
    StartToken :: tokens.:+(EndToken)
  }

  def tokensToString(tokens: Plot): String = {
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
