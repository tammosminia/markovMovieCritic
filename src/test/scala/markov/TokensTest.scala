package markov

import Token._
import markov.Dictionary.DictionaryImpl
import org.scalatest.funsuite.AnyFunSuite

class TokensTest extends AnyFunSuite {
  val ed = DictionaryImpl(Set("hello", "world", "end", "with", "dot", "first", "second", "line"))
  val helloTokens = List(StartToken, WordToken("hello"), EndToken)
  val helloWorldTokens = List(StartToken, WordToken("hello"), WordToken("world"), SignToken("!"), EndToken)
  val endWithDot = List(
    StartToken,
    WordToken("end"),
    WordToken("with"),
    WordToken("dot"),
    SignToken("."),
    EndToken
  )
  val twoLinesTokens = List(
    StartToken,
    WordToken("first"),
    WordToken("line"),
    SignToken("."),
    WordToken("second"),
    WordToken("line"),
    SignToken("."),
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
    assert(tokensToString(helloTokens) === "Hello")
    assert(tokensToString(helloWorldTokens) === "Hello world!")
    assert(tokensToString(endWithDot) === "End with dot.")
    assert(tokensToString(twoLinesTokens) === "First line. Second line.")
  }

  test("weird spacing") {
    assert(tokenize("hello ", ed) === helloTokens)
    assert(tokenize("hello  ", ed) === helloTokens)
    assert(tokenize(" hello", ed) === helloTokens)
    assert(tokenize("  hello", ed) === helloTokens)
    assert(tokenize("  hello  ", ed) === helloTokens)
    assert(tokenize(" ", ed) === List(StartToken, EndToken))
    assert(tokenize("  ", ed) === List(StartToken, EndToken))
    assert(tokenize(" . ", ed) === List(StartToken, SignToken("."), EndToken))
    assert(tokenize("first line   .     second line  .  ", ed) === twoLinesTokens)
    assert(tokenize("first line.second line.", ed) === twoLinesTokens)
  }

  test("words that dont occur in the library") {
    assert(
      tokenize("first unknown word", ed) === List(
        StartToken,
        WordToken("first"),
        InfrequentWord,
        InfrequentWord,
        EndToken
      )
    )
  }

  val plots = List("hello world!", "End, with:dot.", "First line.Second line.")

  test("build dictionary should skip words that occur only once") {
    val d = Dictionary.build(plots, 2)

    assert(d == DictionaryImpl(Set("line")))
  }

  test("build dictionary with all words occurring at least twice") {
    val d = Dictionary.build(plots ++ plots, 2)

    assert(d == ed)
  }

}
