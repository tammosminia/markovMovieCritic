
object MarkovMovieCritic extends App {
  val movies = ReadTestSet.readTestSet
  println(movies.size)

  val moviesByRating: Map[Int, List[Movie]] = movies.groupBy { case Movie(title, plot, rating) => rating }
}