
object MarkovMovieCritic extends App {
  val movies = ReadTestSet.readTestSet(100)
  println(movies.size)

  val moviesByRating: Map[Int, List[Movie]] = movies.groupBy { case Movie(title, plot, rating) => rating }
}