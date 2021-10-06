/** Reads data/plot.list and data/ratings.list
  * Then output all data by movie in data/movies
  */
object ConvertImdbTestSet extends App {
  val movies = ReadTestSet.readImdbTestSet
  ReadTestSet.writeTestSet(movies)
}
