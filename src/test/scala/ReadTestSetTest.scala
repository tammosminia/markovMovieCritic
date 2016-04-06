class ReadTestSetTest extends org.scalatest.FunSuite {
  import ReadTestSet._

  test("reg") {
    assert("      0000000125  1634308   9.2  The Shawshank Redemption (1994)".matches(ratingsRegex.regex))
  }

  test("reg extract The Shawshank Redemption") {
    val ratingsRegex(rating, title) = "      0000000125  1634308   9.2  The Shawshank Redemption (1994)"
    assert(rating === "9.2")
    assert(title === "The Shawshank Redemption (1994)")
  }

  test("reg extract 100 Years of Horror") {
    val ratingsRegex(rating, title) = """      0...101203      15   7.6  "100 Years of Horror" (1996) {100 Years of Horror: Gory Gimmicks (#1.18)}"""
    assert(rating === "7.6")
    assert(title === """"100 Years of Horror" (1996) {100 Years of Horror: Gory Gimmicks (#1.18)}""")
  }

}
