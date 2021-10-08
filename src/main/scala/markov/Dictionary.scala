package markov

import markov.Token.{ignoredSignsRegex, tokenSignsRegex}

trait Dictionary {
  def isFrequentWord(w: String): Boolean
}

object Dictionary {
  case class DictionaryImpl(words: Set[String]) extends Dictionary {
    def isFrequentWord(w: String): Boolean = words.contains(w)

    override def toString: String = s"Dictionary of ${words.size} words"
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
