package markov

import Tokens._
import org.scalatest.funsuite.AnyFunSuite

class TokensTest extends AnyFunSuite {
  val helloTokens = List(StartToken, WordToken("hello"), EndSentence, EndToken)
  val helloWorldTokens = List(
    StartToken,
    WordToken("hello"),
    WordToken("world"),
    EndSentence,
    EndToken
  )
  val endWithDot = List(
    StartToken,
    WordToken("end"),
    WordToken("with"),
    WordToken("dot"),
    EndSentence,
    EndToken
  )
  val twoLinesTokens = List(
    StartToken,
    WordToken("first"),
    WordToken("line"),
    EndSentence,
    WordToken("second"),
    WordToken("line"),
    EndSentence,
    EndToken
  )

  test("tokenize") {
    assert(tokenize("hello") === helloTokens)
    assert(tokenize("Hello world!") === helloWorldTokens)
    assert(tokenize("End with dot.") === endWithDot)
    assert(tokenize("First line. Second line.") === twoLinesTokens)
    assert(tokenize("First line.\n Second line.\n") === twoLinesTokens)
  }

  test("tokensToString") {
    assert(tokensToString(helloTokens) === "Hello.")
    assert(tokensToString(helloWorldTokens) === "Hello world.")
    assert(tokensToString(endWithDot) === "End with dot.")
    assert(tokensToString(twoLinesTokens) === "First line. Second line.")
  }

  test("multiple dots") {
    assert(tokenize("first line .. second line") === twoLinesTokens)
    assert(tokenize("first line ..... second line......") === twoLinesTokens)
  }

  test("weird spacing") {
    assert(tokenize("hello ") === helloTokens)
    assert(tokenize("hello  ") === helloTokens)
    assert(tokenize(" hello") === helloTokens)
    assert(tokenize("  hello") === helloTokens)
    assert(tokenize("  hello  ") === helloTokens)
    assert(tokenize(" ") === List(StartToken, EndSentence, EndToken))
    assert(tokenize("  ") === List(StartToken, EndSentence, EndToken))
    assert(tokenize(" . ") === List(StartToken, EndSentence, EndToken))
    assert(tokenize("first line   .  .   second line") === twoLinesTokens)
  }

}
