import MarkovModel._

object MarkovMovieCritic extends App {
  val movies = ReadTestSet.readTestSet(10000)
  println(movies.size)

  val moviesByRating: Map[Int, List[Movie]] = movies.groupBy { case Movie(title, plot, rating) => rating }

  val tokens = movies.map(movie => MarkovModel.tokenize(movie.plot))
  val model = MarkovModel.learn(tokens)
//  model.print()
  val plot = model.generateRandomPlot()
  println(tokensToString(plot))
}