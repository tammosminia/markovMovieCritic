class ReadTestSetIntegrationTest extends org.scalatest.FunSuite {
  import ReadTestSet._

  val movies = readImdbTestSet

  test("The Holy Mountain (1973)") {
    val movie = movies.find(_.title == "The Holy Mountain (1973)")
    assert(movie.nonEmpty)
    assert(movie.get.plot === "A Christlike figure wanders through bizarre, grotesque scenarios filled with religious and sacrilegious imagery. He meets a mystical guide who introduces him to seven wealthy and powerful people, each representing a planet in the Solar system. These seven, along with the protagonist, the guide and the guide's assistant, divest themselves of their worldly goods and form a group of nine who will seek the Holy Mountain, in order to displace the gods who live there and become immortal.")
    assert(movie.get.rating === 4)
  }

  test("Fear and Loathing in Las Vegas (1998)") {
    val movie = movies.find(_.title == "Fear and Loathing in Las Vegas (1998)")
    assert(movie.nonEmpty)
    assert(movie.get.plot === "The big-screen version of Hunter S. Thompson's seminal psychedelic classic about his road trip across Western America as he and his large Samoan lawyer searched desperately for the \"American dream\"... they were helped in large part by the huge amount of drugs and alcohol kept in their convertible, The Red Shark.")
    assert(movie.get.rating === 4)
  }


}
