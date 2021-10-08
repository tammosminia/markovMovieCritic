import MovieSet.UnparsedMovie

import scala.io.Source

/** Reads data/plot.list and data/ratings.list
  * Then output all data by movie in data/movies
  */
object ConvertImdbMovies extends App {
  val movies = ImdbMovies.readImdbMovies
  MovieSet.write(movies)
}

object ImdbMovies {
  val ratingsRegex = """^\s+[\d\.]+\s+\d+\s+(\d+\.\d+)\s+(.+)$""".r

  def mapRating(s: String): Int = {
    ((s.toDouble - 0.01) / 2).toInt + 1
  }

  def readImdbMovies: List[UnparsedMovie] = {
    var plots: Map[String, String] = Map()
    var lastMovie: Option[String] = None
    Source.fromFile("data/plot.list", "ISO-8859-15").getLines.foreach {
      case line if line.startsWith("MV:") =>
        lastMovie = Some(line.stripPrefix("MV:").trim)
      case line if line.startsWith("PL:") && lastMovie.nonEmpty =>
        val plotLine = line.stripPrefix("PL:").trim
        val newPlot = plots.get(lastMovie.get) match {
          case None          => plotLine
          case Some(oldPlot) => oldPlot + " " + plotLine
        }
        plots = plots.updated(lastMovie.get, newPlot)
      case line if line.startsWith("BY:") && lastMovie.nonEmpty =>
        lastMovie = None
      case _ => //nothing
    }

    var ratings: Map[String, Int] = Map()
    Source.fromFile("data/ratings.list", "ISO-8859-15").getLines.dropWhile(!_.equals("MOVIE RATINGS REPORT")).foreach {
      case ratingsRegex(rating, title) =>
        val ratingInStars = mapRating(rating)
        ratings = ratings.updated(title, ratingInStars)
      //        println(s"added $title, $ratingInStars stars")
      case _ =>
    }

    plots.flatMap { case (title, plot) =>
      ratings.get(title).map { rating =>
        UnparsedMovie(title, plot, rating)
      }
    }.toList
  }
}
