import markov.{Dictionary, Movie, MovieClassifier}
import markov.Token._

object MarkovMovieCritic extends App {
  val movies = MovieSet.readMovies(1)
  println(s"read movieSet $movies")

  println(s"learning models")
  val classifier = MovieClassifier.learn(movies.learnSet)

  val predictions = movies.testSet.map { movie =>
    val ratingPrediction = classifier.predict(movie.plot)
    (movie, ratingPrediction)
  }
  val correctPredictions = predictions.count { case (m, p) => m.rating == p }
  val accuracy = correctPredictions.toDouble / predictions.length
  println(s"correctly predicted $correctPredictions ratings from ${predictions.length} test movies ($accuracy)")

  println("generating plots for all ratings")
  classifier.modelsByRating.toList.sortBy(_._1).foreach { case (rating, model) =>
    val plot = model.generateRandomPlot()
    println(s"$rating stars: ${tokensToString(plot)}")
  }
}
