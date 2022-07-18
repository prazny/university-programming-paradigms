sealed trait Graphs[+A]

case class Graph[A](value: A, nodes: List[Graph[A]]) extends Graphs[A]

sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


def graphToNode[A](g: Graph[A]): Node[A] =
  if g.nodes.length > 2 then graphToNode(Graph(g.nodes.head.value, g.nodes.head.nodes ::: List(Graph(g.value, g.nodes.tail))))
  else g match
    case Graph(value, Nil) => Node(value, Empty, Empty)
    case Graph(value, h :: Nil) => Node(value, graphToNode(h), Empty)
    case Graph(value, h :: t) => Node(value, graphToNode(h), graphToNode(t.head))


val g = Graph(5, List(Graph(2, List(Graph(61, List()), Graph(66, List()))), Graph(6, List()), Graph(3, List())))
val g2 = Graph(1, List(Graph(2, List()), Graph(3, List()), Graph(4, List())))
graphToNode(g)
graphToNode(g2)
