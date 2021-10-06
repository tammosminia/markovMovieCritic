package markov

import Tokens._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration.Workers

class TokensTest extends AnyFunSuite {
  val ed = Dictionary(Set("hello", "world", "end", "with", "dot", "first", "second", "line"))
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
    assert(tokenize("hello", ed) === helloTokens)
    assert(tokenize("Hello world!", ed) === helloWorldTokens)
    assert(tokenize("End with dot.", ed) === endWithDot)
    assert(tokenize("First line.Second line.", ed) === twoLinesTokens)
    assert(tokenize("First line. Second line.", ed) === twoLinesTokens)
    assert(tokenize("First line.\n Second line.\n", ed) === twoLinesTokens)
  }

  test("tokensToString") {
    assert(tokensToString(helloTokens) === "Hello.")
    assert(tokensToString(helloWorldTokens) === "Hello world.")
    assert(tokensToString(endWithDot) === "End with dot.")
    assert(tokensToString(twoLinesTokens) === "First line. Second line.")
  }

  test("multiple dots") {
    assert(tokenize("first line .. second line", ed) === twoLinesTokens)
    assert(tokenize("first line ..... second line......", ed) === twoLinesTokens)
  }

  test("weird spacing") {
    assert(tokenize("hello ", ed) === helloTokens)
    assert(tokenize("hello  ", ed) === helloTokens)
    assert(tokenize(" hello", ed) === helloTokens)
    assert(tokenize("  hello", ed) === helloTokens)
    assert(tokenize("  hello  ", ed) === helloTokens)
    assert(tokenize(" ", ed) === List(StartToken, EndSentence, EndToken))
    assert(tokenize("  ", ed) === List(StartToken, EndSentence, EndToken))
    assert(tokenize(" . ", ed) === List(StartToken, EndSentence, EndToken))
    assert(tokenize("first line   .  .   second line", ed) === twoLinesTokens)
  }

  test("words that dont occur in the library") {
    assert(
      tokenize("first unknown word", ed) === List(
        StartToken,
        WordToken("first"),
        InfrequentWord,
        InfrequentWord,
        EndSentence,
        EndToken
      )
    )
  }

  val plots = List("hello world!", "End, with:dot.", "First line.Second line.")

  test("build dictionary should skip words that occur only once") {
    val d = Dictionary.build(plots)

    assert(d == Dictionary(Set("line")))
  }

  test("build dictionary with all words occurring at least twice") {
    val d = Dictionary.build(plots ++ plots)

    assert(d == ed)
  }

}
