import markov.MovieClassifier
import markov.Token._

object MarkovMovieCritic extends App {
  val movies = MovieSet.readMovies(10)
  println(s"read movieSet $movies")

  println(s"learning models")
  val classifier = MovieClassifier.learn(movies.learnSet)

  println(s"accuracy on learnSet: ${classifier.testAccuracy(movies.learnSet)}")
  println(s"accuracy on testSet: ${classifier.testAccuracy(movies.testSet)}")

  println(s"Dune: ${classifier.predict(
    tokenize(
      "Feature adaptation of Frank Herbert's science fiction novel, about the son of a noble family entrusted with the protection of the most valuable asset and most vital element in the galaxy.",
      movies.dictionary
    )
  )}")
  println(s"good programmer: ${classifier.predict(
    tokenize(
      "A programmer writes great code.",
      movies.dictionary
    )
  )}")
  println(s"bad programmer: ${classifier.predict(
    tokenize(
      "The programmer forgets to write tests",
      movies.dictionary
    )
  )}")
  println(s"MasterChef: ${classifier.predict(
    tokenize(
      "In this show a group of chefs will compete in a series of challenges (cooking) to impress the judges. And to be crowned MasterChef",
      movies.dictionary
    )
  )}")
  println(s"Boerenpsalm: ${classifier.predict(
    tokenize(
      "This Flemish heimat-movie after a novel by Felix Timmermans, the title of which translates as \"farmers psalm\", is about the at first sight idyllic, but actually tragical life of Wortel, a god-fearing peasant in a pre-war Belgian village, who has a hard time accepting the curate's assurance it's all part of God's grand plan when his loved ones keep coming to terrible harm, and searching solace in the hand-carving of a wooden statue of Christ.",
      movies.dictionary
    )
  )}")
  println(s"auditie: ${classifier.predict(
    tokenize(
      "In a concentration camp a young boy is auditioning for the camp s orchestra. He leaves his father behind and walks the long and cold distance to the place what really is an execution site. While he is playing he escapes in a fantasy world. At the last note he returns to the harsh reality and stands waiting for the decision of the the firing squad. His father listens from a distance.",
      movies.dictionary
    )
  )}")
  println(s"fout: ${classifier.predict(
    tokenize(
      "dit klopt niet",
      movies.dictionary
    )
  )}")
  println(s"wrong: ${classifier.predict(
    tokenize(
      "this goes wrong",
      movies.dictionary
    )
  )}")

  println("generating plots for all ratings")
  classifier.chains.toList.sortBy(_._1).foreach { case (rating, model) =>
    val plot = model.generateRandomPlot()
    println(s"$rating stars: ${tokensToString(plot)}")
  }
}
