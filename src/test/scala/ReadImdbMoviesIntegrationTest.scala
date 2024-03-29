import org.scalatest.funsuite.AnyFunSuite

class ReadImdbMoviesIntegrationTest extends AnyFunSuite {
  val movies = ImdbMovies.readImdbMovies

  test("Fear and Loathing in Las Vegas (1998)") {
    val movie = movies.find(_.title == "Fear and Loathing in Las Vegas (1998)")
    assert(movie.nonEmpty)
    assert(
      movie.get.plot === "The big-screen version of Hunter S. Thompson's seminal psychedelic classic about his road trip across Western America as he and his large Samoan lawyer searched desperately for the \"American dream\"... they were helped in large part by the huge amount of drugs and alcohol kept in their convertible, The Red Shark."
    )
    assert(movie.get.rating === 4)
  }

}
