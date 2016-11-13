import MarkovModel.{EndToken, StartToken, WordToken}
import org.scalatest.FunSuite

class MarkovModelTest extends FunSuite {
  val helloSentence = MarkovModel.tokenize("hello")
  val helloWorldSentence = MarkovModel.tokenize("Hello world!")


  test("tokenize") {
    assert(helloSentence === List(StartToken, WordToken("hello"), EndToken))
    assert(helloWorldSentence === List(StartToken, WordToken("hello"), WordToken("world!"), EndToken))
  }

}
