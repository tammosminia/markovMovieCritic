package markov

import markov.MarkovModel.{Link, Links}
import markov.Tokens.{Dictionary, EndToken, StartToken, WordToken}
import org.scalatest.funsuite.AnyFunSuite

class MarkovModelTest extends AnyFunSuite {
  test("learning The cat sits on the mat") {
    val plot = Tokens.tokenize("The cat sits on the mat", Dictionary.AllWords)

    val r = MarkovModel.learn(List(plot))

    assert(
      r == MarkovModel(
        Map(
          StartToken -> Links(List(Link(WordToken("the"), 1))),
          WordToken("the") -> Links(List(Link(WordToken("cat"), 1), Link(WordToken("mat"), 1))),
          WordToken("cat") -> Links(List(Link(WordToken("sits"), 1))),
          WordToken("sits") -> Links(List(Link(WordToken("on"), 1))),
          WordToken("on") -> Links(List(Link(WordToken("the"), 1))),
          WordToken("mat") -> Links(List(Link(EndToken, 1)))
        )
      )
    )
  }
}
