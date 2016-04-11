object MarkovModel {
  abstract class Token
  case class WordToken(word: String) extends Token
  object EndOfLine extends Token
  object EndToken extends Token

  case class Link(to: Token, count: Int)

//  abstract class AbstractNode
//  case class Node(token: Token, links: List[Link]) extends AbstractNode
//  object EndNode extends AbstractNode


  def tokenize(plot: String): List[Token] = {
    val separatedDots = plot.replace(".", " . ")
    val words = separatedDots.toLowerCase.split("""\s+""").toList
    val tokens = words.map {
      case "." => EndOfLine
      case word => WordToken(word)
    }
    tokens.:+(EndToken)
  }

  def learn(learnSet: List[List[Token]]): MarkovModel = {
    val tuples: List[List[Token]] = learnSet.flatMap { tokens =>
      tokens.sliding(2).toList
    }
    val transitions: Map[Token, List[Token]] = tuples.groupBy(_.head).mapValues(tuples => tuples.map(_(1)))
    val countedTransitions: Map[Token, List[Link]] = transitions.mapValues { toTokens: List[Token] =>
      val grouped: Map[Token, List[Token]] = toTokens.groupBy(token => token)
      grouped.map {
        case (toToken, list) => Link(toToken, list.size)
      }.toList
    }
    new MarkovModel(countedTransitions)
  }
}

import MarkovModel._

class MarkovModel(map: Map[Token, List[Link]]) {
  def print() = {
    map.foreach {
      case (token, links) =>
        println(token)
        links.foreach { link =>
          println(s"  -> ${link.to} ${link.count}")
        }
    }
  }
}
