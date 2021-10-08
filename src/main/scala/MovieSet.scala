import markov.Token.tokenize
import markov.{Dictionary, Movie, Rating}

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.Random

case class MovieSet(learnSet: List[Movie], testSet: List[Movie], dictionary: Dictionary) {
  override def toString: String = s"MovieSet with ${learnSet.size} learnSet, ${testSet.size} testSet, $dictionary"
}

object MovieSet {
  case class UnparsedMovie(title: String, plot: String, rating: Rating) {
    assert(rating >= 1 && rating <= 5, "rating should be in stars, from 1(horrible) to 5(great)")
  }

  def write(movies: List[UnparsedMovie]) = {
    val writer = new PrintWriter(new File("data/movies"))
    writer.println(s"Total ${movies.size}")
    Random.shuffle(movies).foreach { movie =>
      writer.println(movie.title)
      writer.println(movie.plot)
      writer.println(movie.rating)
      writer.println()
    }
    writer.close()
  }

  def readRaw(amount: Int): List[UnparsedMovie] = {
    Source
      .fromFile("data/movies")
      .getLines
      .drop(1)
      .grouped(4)
      .take(amount)
      .map { lines: Seq[String] =>
        UnparsedMovie(lines(0), lines(1), lines(2).toInt)
      }
      .toList
  }

  def readMovies(fraction: Int): MovieSet = {
    val totalTestSet = 283664
    val inputMovies = readRaw(totalTestSet / fraction)
    val dictionary = Dictionary.build(inputMovies.map(_.plot), 5)
    val allMovies: List[Movie] = Random.shuffle(inputMovies.map { m =>
      Movie(m.title, tokenize(m.plot, dictionary), m.rating)
    })
    val learnSet = allMovies.take((allMovies.size * 0.9).toInt)
    val testSet = allMovies.drop((allMovies.size * 0.1).toInt)
    MovieSet(learnSet, testSet, dictionary)
  }

}
