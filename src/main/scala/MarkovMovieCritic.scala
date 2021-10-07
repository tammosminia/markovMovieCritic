import markov.MarkovModel
import markov.Tokens._

object MarkovMovieCritic extends App {
  val totalTestSet = 283664
  val movies = ReadTestSet.readTestSet(totalTestSet)
  val moviesLearnSet = movies.take((movies.size * 0.9).toInt)
  val moviesTestSet = movies.drop((movies.size * 0.1).toInt)
  val dictionary = Dictionary.build(movies.map(_.plot), 5)
  println(s"dictionary contains ${dictionary.words.size} words")

  println(s"learning models from ${moviesLearnSet.length} examples")
  val moviesByRating: Map[Int, List[Movie]] = moviesLearnSet.groupBy(_.rating)
  val modelsByRating = moviesByRating.map { case (rating, ms) =>
    val tokens = ms.map(movie => tokenize(movie.plot, dictionary))
    val model = MarkovModel.learn(tokens)
    (rating, model)
  }

  println("generating plots for all ratings")
  modelsByRating.toList.sortBy(_._1).foreach { case (rating, model) =>
//    model.print()
    val plot = model.generateRandomPlot()
    println(s"$rating stars: ${tokensToString(plot)}")
  }

  val predictions = moviesTestSet.map { movie =>
    val tokens = tokenize(movie.plot, dictionary)
    val ratingPrediction = modelsByRating.maxBy { case (_, model) =>
      model.probabilityToOutput(tokens)
    }._1
    (movie, ratingPrediction)
  }
  val correctPredictions = predictions.count { case (m, p) => m.rating == p }
  println(
    s"correctly predicted $correctPredictions ratings from ${predictions.length} test movies (${correctPredictions.toDouble / predictions.length})"
  )
}
