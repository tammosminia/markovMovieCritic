package object markov {
  type Plot = List[Token]
  type Rating = Int

  case class Movie(title: String, plot: Plot, rating: Rating)
}
