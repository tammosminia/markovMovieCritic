import markov.MarkovModel
import markov.Tokens._

object MarkovMovieCritic extends App {
  val movies = ReadTestSet.readTestSet(283664)
  println(movies.size)

  val moviesLearnSet = movies.take((movies.size * 0.9).toInt)
  val moviesTestSet = movies.drop((movies.size * 0.1).toInt)

  val moviesByRating: Map[Int, List[Movie]] = moviesLearnSet.groupBy {
    case Movie(title, plot, rating) => rating
  }

  val modelsByRating = moviesByRating.map { case (rating, m) =>
    val tokens = m.map(movie => tokenize(movie.plot))
    val model = MarkovModel.learn(tokens)
    (rating, model)
  }

  modelsByRating.foreach { case (rating, model) =>
    println(s"Rating $rating")
//    model.print()
    val plot = model.generateRandomPlot()
    println(tokensToString(plot))
  }

//  val amountCorrect = moviesTestSet.count { movie =>
//    val tokens = MarkovModel.tokenize(movie.plot)
//    val ratingProbabilities = modelsByRating.map { case (rating, model) =>
//
//    }
//  }

}
