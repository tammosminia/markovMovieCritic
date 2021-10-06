package markov

import scala.util.Random
import Tokens._

object MarkovModel {
  case class Link(to: Token, count: Int)
  class Links(links: List[Link]) {
    val linksMap: Map[Token, Int] = links
      .map { case Link(token, count) => token -> count }
      .toMap
      .withDefaultValue(0)

    val totalCount = links.map(_.count).sum
    val totalCountMin = links.filterNot(_.to == InfrequentWord).map(_.count).sum

    def print() = links.foreach { link =>
      println(s"  -> ${link.to} ${link.count}")
    }

    def randomTo: Token = {
      if (totalCountMin == 0) return EndToken
      var random = Random.nextInt(totalCountMin)
      links.foreach { link =>
        if (link.to != InfrequentWord) random -= link.count
        if (random < 0) return link.to
      }
      throw new RuntimeException("randomTo")
    }

    def probabilityToOutput(token: Token): Double =
      linksMap(token).toDouble / totalCount
  }

  def learn(learnSet: List[List[Token]]): MarkovModel = {
    val tuples: List[List[Token]] = learnSet.flatMap { tokens =>
      tokens.sliding(2).toList
    }
    val transitions: Map[Token, List[Token]] =
      tuples.groupBy(_.head).mapValues(tuples => tuples.map(_(1))).toMap
    val countedTransitions: Map[Token, List[Link]] = transitions.mapValues { toTokens: List[Token] =>
      val grouped: Map[Token, List[Token]] = toTokens.groupBy(token => token)
      grouped.map { case (toToken, list) =>
        Link(toToken, list.size)
      }.toList
    }.toMap
    val map = countedTransitions.mapValues(new Links(_)).toMap
    new MarkovModel(map)
  }

}

import markov.MarkovModel._

class MarkovModel(map: Map[Token, Links]) {
  def print() = {
    map.foreach { case (token, links) =>
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
    case List()         => 0.0
    case List(EndToken) => 1.0
    case head :: tail =>
      map
        .get(head)
        .orElse(map.get(InfrequentWord)) //if this token has not been learned on
        .map(_.probabilityToOutput(tail.head) * probabilityToOutput(tail))
        .getOrElse(0.0) //if it also has not learned InfrequentWord
  }
}
