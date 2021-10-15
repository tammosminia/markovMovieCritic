import markov.MovieClassifier
import markov.Token._

object MarkovMovieCritic extends App {
  val movies = MovieSet.readMovies(1)
  println(s"read movieSet $movies")

  println(s"learning models")
  val classifier = MovieClassifier.learn(movies.learnSet)

  println(s"accuracy on learnSet: ${classifier.testAccuracy(movies.learnSet)}")
  println(s"accuracy on testSet: ${classifier.testAccuracy(movies.testSet)}")

  println("generating plots for all ratings")
  classifier.chains.toList.sortBy(_._1).foreach { case (rating, model) =>
    val plot = model.generateRandomPlot()
    println(s"$rating stars: ${tokensToString(plot)}")
  }
}
