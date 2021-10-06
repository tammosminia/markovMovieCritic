import org.scalatest.funsuite.AnyFunSuite

class ReadTestSetTest extends AnyFunSuite {
  import ReadTestSet._

  test("reg") {
    assert(
      "      0000000125  1634308   9.2  The Shawshank Redemption (1994)"
        .matches(ratingsRegex.regex)
    )
  }

  test("reg extract The Shawshank Redemption") {
    val ratingsRegex(rating, title) =
      "      0000000125  1634308   9.2  The Shawshank Redemption (1994)"
    assert(rating === "9.2")
    assert(title === "The Shawshank Redemption (1994)")
  }

  test("reg extract 100 Years of Horror") {
    val ratingsRegex(rating, title) =
      """      0...101203      15   7.6  "100 Years of Horror" (1996) {100 Years of Horror: Gory Gimmicks (#1.18)}"""
    assert(rating === "7.6")
    assert(
      title === """"100 Years of Horror" (1996) {100 Years of Horror: Gory Gimmicks (#1.18)}"""
    )
  }

  test("mapRating") {
    assert(mapRating("0.0") === 1)
    assert(mapRating("0.1") === 1)
    assert(mapRating("2.0") === 1)
    assert(mapRating("2.1") === 2)
    assert(mapRating("3.9") === 2)
    assert(mapRating("4.0") === 2)
    assert(mapRating("4.1") === 3)
    assert(mapRating("6.1") === 4)
    assert(mapRating("7.7") === 4)
    assert(mapRating("8.0") === 4)
    assert(mapRating("8.1") === 5)
    assert(mapRating("10.0") === 5)
  }

}
