object MarkovModel {
  case class Link(to: Node, count: Int)
  case class Node(word: String, links: List[Link])

}
