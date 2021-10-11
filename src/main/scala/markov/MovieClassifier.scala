package markov

case class MovieClassifier(chains: Map[Rating, MarkovChain]) {
  def predict(plot: Plot): Rating =
    chains.maxBy { case (_, chain) =>
      chain.probabilityToOutput(plot)
    }._1

  def testAccuracy(movies: List[Movie]): Double = {
    val predictions = movies.map { movie =>
      (movie, predict(movie.plot))
    }
    val correctPredictions = predictions.count { case (m, p) => m.rating == p }
    val inCorrectPredictions = predictions.filterNot { case (m, p) => m.rating == p }
    Range(1, 6).foreach { r =>
      println(s"incorrectly predicted $r: ${inCorrectPredictions.count(_._2 == r)}")
      println(s"misrated movies with original rating $r: ${inCorrectPredictions.count(_._1.rating == r)}")
    }
    correctPredictions.toDouble / predictions.length
  }
}

object MovieClassifier {
  def learn(moviesLearnSet: List[Movie]): MovieClassifier = {
    val modelsByRating: Map[Rating, MarkovChain] = moviesLearnSet
      .groupBy(_.rating)
      .map { case (rating, ms) =>
        val model = MarkovChain.learn(ms.map(_.plot))
        (rating, model)
      }
    MovieClassifier(modelsByRating)
  }
}
