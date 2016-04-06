import scala.io.Source

case class Movie(title: String, plot: String, rating: Int)

object ReadTestSet {
  val ratingsRegex = """^\s+[\d\.]+\s+\d+\s+(\d+\.\d+)\s+(.+)$""".r

  def mapRating(s: String): Int = {
    (s.toDouble / 4).toInt + 1
  }

  def readTestSet: List[Movie] = {
    var plots: Map[String, String] = Map().withDefaultValue("")
    var lastMovie = "nothing"
    Source.fromFile("plot.list", "ISO-8859-15").getLines.foreach { line =>
      if (line.startsWith("MV:")) {
        lastMovie = line.stripPrefix("MV:").trim
        println(s"added $lastMovie total:${plots.keySet.size}")
      } else if (line.startsWith("PL:")) {
        plots = plots.updated(lastMovie, plots(lastMovie) + line.stripPrefix("PL").trim)
      }
    }

    var ratings: Map[String, Int] = Map()
    Source.fromFile("ratings.list", "ISO-8859-15").getLines.dropWhile(!_.equals("MOVIE RATINGS REPORT")).foreach { line =>
      line match {
        case ratingsRegex(rating, title) =>
          val ratingInStars = mapRating(rating)
          ratings = ratings.updated(title, ratingInStars)
          println(s"added $title, $ratingInStars stars")
        case _ =>
      }
    }

    plots.flatMap { case (title, plot) =>
      ratings.get(title).map { rating =>
        Movie(title, plot, rating) }
    }.toList
  }

}
