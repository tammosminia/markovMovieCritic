package markov

import scala.util.Random
import Token._

import markov.MarkovModel._

case class MarkovModel(map: Map[Token, Links]) {
  def print(): Unit = {
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

object MarkovModel {
  case class Link(to: Token, count: Int)
  case class Links(links: List[Link]) {
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

  def learn(learnSet: List[Plot]): MarkovModel = {
    // Create a list of all transitions from one Token to another, including doubles
    val tuples: List[(Token, Token)] = learnSet.flatMap { plot =>
      plot.sliding(2).toList.map { case List(from, to) => (from, to) }
    }
    val transitions: Map[Token, List[Token]] =
      tuples.groupBy(_._1).map { case (from, tuples) => (from, tuples.map(_._2)) }
    // Count how often transitions occur from Token X to Token Y for all X and Y
    val countedTransitions: Map[Token, Links] = transitions.map { case (from, toTokens: List[Token]) =>
      val tos = toTokens.groupBy(identity).map { case (toToken, allSimilar) => Link(toToken, allSimilar.size) }
      (from, Links(tos.toList))
    }
    MarkovModel(countedTransitions)
  }

}
