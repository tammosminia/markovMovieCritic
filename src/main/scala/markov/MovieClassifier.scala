package markov

case class MovieClassifier(modelsByRating: Map[Rating, MarkovModel]) {
  def predict(plot: Plot): Rating =
    modelsByRating.maxBy { case (_, model) =>
      model.probabilityToOutput(plot)
    }._1
}

object MovieClassifier {
  def learn(moviesLearnSet: List[Movie]): MovieClassifier = {
    val modelsByRating: Map[Rating, MarkovModel] = moviesLearnSet
      .groupBy(_.rating)
      .map { case (rating, ms) =>
        val model = MarkovModel.learn(ms.map(_.plot))
        (rating, model)
      }
    MovieClassifier(modelsByRating)
  }

}
