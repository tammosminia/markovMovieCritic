import scala.util.Random

object MarkovModel {
  abstract class Token
  case class WordToken(word: String) extends Token
  object EndOfLine extends Token
  object StartToken extends Token
  object EndToken extends Token

  case class Link(to: Token, count: Int)
  class Links(var links: List[Link]) {
//    links = links.filter(_.count > 1)
    val linksMap: Map[Token, Int] = links.map { case Link(token, count) => token -> count }.toMap.withDefaultValue(0)

    val totalCount = links.map(_.count).sum

    def print() = links.foreach { link =>
      println(s"  -> ${link.to} ${link.count}")
    }

    def randomTo: Token = {
      var random = Random.nextInt(totalCount)
      links.foreach { link =>
        random -= link.count
        if (random < 0) return link.to
      }
      throw new RuntimeException("randomTo")
    }

    def probabilityToOutput(token: Token): Double = linksMap(token).toDouble / totalCount
  }


  def tokenize(plot: String): List[Token] = {
    val dropSigns = plot.replaceAll("""[,:;]""", "")
    val separatedDots = dropSigns.replace(".", " . ")
    val words = separatedDots.toLowerCase.split("""\s+""").toList
    val tokens = words.map {
      case "." => EndOfLine
      case word => WordToken(word)
    }
    StartToken :: tokens.:+(EndToken)
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
    val map = countedTransitions.mapValues(new Links(_))
    new MarkovModel(map)
  }

  def tokensToString(tokens: List[Token]): String = {
    val noMargins = tokens.dropWhile(_ == StartToken).takeWhile(_ != EndToken)
    noMargins.map {
      case EndOfLine => "."
      case WordToken(word) => word
    }.mkString(" ").replace(" . ", ".\n")
  }
}

import MarkovModel._

import scala.util.Random

class MarkovModel(map: Map[Token, Links]) {
  def print() = {
    map.foreach {
      case (token, links) =>
        println(token)
        links.print()
    }
  }

  def generateRandomPlot(fromToken: Token = StartToken): List[Token] = {
    if (fromToken == EndToken) return List(EndToken)
    val links = map(fromToken)
    val nextToken = links.randomTo
    fromToken :: generateRandomPlot(nextToken)
  }

  def probabilityToOutput(tokens: List[Token]): Double = tokens match {
    case List() => 0.0
    case List(EndToken) => 1.0
    case head :: tail => map(head).probabilityToOutput(tail.head) * probabilityToOutput(tail)
  }
}
