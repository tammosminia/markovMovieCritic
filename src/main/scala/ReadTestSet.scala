import java.io.{File, PrintWriter}

import scala.io.Source
import scala.util.Random

case class Movie(title: String, plot: String, rating: Int)

object ReadTestSet {
  val ratingsRegex = """^\s+[\d\.]+\s+\d+\s+(\d+\.\d+)\s+(.+)$""".r

  def mapRating(s: String): Int = {
    (s.toDouble / 4).toInt + 1
  }

  def readImdbTestSet: List[Movie] = {
    var plots: Map[String, String] = Map().withDefaultValue("")
    var lastMovie = "nothing"
    Source.fromFile("data/plot.list", "ISO-8859-15").getLines.foreach { line =>
      if (line.startsWith("MV:")) {
        lastMovie = line.stripPrefix("MV:").trim
        println(s"added $lastMovie total:${plots.keySet.size}")
      } else if (line.startsWith("PL:")) {
        plots = plots.updated(lastMovie, plots(lastMovie) + line.stripPrefix("PL").trim)
      }
    }

    var ratings: Map[String, Int] = Map()
    Source.fromFile("data/ratings.list", "ISO-8859-15").getLines.dropWhile(!_.equals("MOVIE RATINGS REPORT")).foreach {
      case ratingsRegex(rating, title) =>
        val ratingInStars = mapRating(rating)
        ratings = ratings.updated(title, ratingInStars)
        println(s"added $title, $ratingInStars stars")
      case _ =>
    }

    plots.flatMap { case (title, plot) =>
      ratings.get(title).map { rating =>
        Movie(title, plot, rating) }
    }.toList
  }

  def writeTestSet(movies: List[Movie]) = {
    val writer = new PrintWriter(new File("data/movies" ))
    writer.println(s"Total ${movies.size}")
    Random.shuffle(movies).foreach { movie =>
      writer.println(movie.title)
      writer.println(movie.plot)
      writer.println(movie.rating)
      writer.println()
    }
    writer.close()
  }

  def readTestSet(amount: Int): List[Movie] = {
    Source.fromFile("data/movies").getLines.drop(1).grouped(4).take(amount).map { lines: Seq[String] =>
      Movie(lines(0), lines(1), lines(2).toInt)
    }.toList
  }

}
