package markov

import scala.util.matching.Regex

object Tokens {
  sealed abstract class Token
  case class WordToken(word: String) extends Token
  case object InfrequentWord extends Token
  case class SignToken(sign: String) extends Token
  case object StartToken extends Token
  case object EndToken extends Token

  val tokenSignsRegex: Regex = """([(),;!?.])""".r //These become tokens
  val ignoredSignsRegex: Regex = """[:"&]""".r //We drop these

  trait Dictionary {
    def isFrequentWord(w: String): Boolean
  }

  object Dictionary {
    case class DictionaryImpl(words: Set[String]) extends Dictionary {
      def isFrequentWord(w: String): Boolean = words.contains(w)
    }

    object AllWords extends Dictionary {
      override def isFrequentWord(w: String): Boolean = true
    }

    def build(plots: List[String], minCount: Int): DictionaryImpl = {
      val allWords = plots.flatMap { p =>
        val withoutSigns = tokenSignsRegex.replaceAllIn(ignoredSignsRegex.replaceAllIn(p, " "), " ")
        withoutSigns.toLowerCase.split("""\s+""").toList
      }
      val frequentWords: Set[String] = allWords
        .groupBy(identity)
        .toList
        .collect {
          case (w, l) if l.length >= minCount => w
        }
        .toSet
      DictionaryImpl(frequentWords)
    }
  }

  type Plot = List[Token]

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
