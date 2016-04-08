object ConvertImdbTestSet extends App {
  val movies = ReadTestSet.readImdbTestSet
  ReadTestSet.writeTestSet(movies)
}
