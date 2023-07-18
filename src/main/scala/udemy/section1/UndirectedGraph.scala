package udemy.section1

import scala.collection.immutable.Map

class UndirectedGraph[V](adjacencyList: Map[V, List[V]]) extends DirectedGraph[V](adjacencyList) {
  override def addEdge(a: V, b: V): UndirectedGraph[V] = {
    val aNeighbors = adjacencyList.getOrElse(a, Nil)
    val bNeighbors = adjacencyList.getOrElse(b, Nil)

    if (aNeighbors.contains(b)) new UndirectedGraph(adjacencyList)
    else {
      new UndirectedGraph(
        adjacencyList +
          (a -> (aNeighbors :+ b), b -> (bNeighbors :+ a))
      )
    }
  }
}

object TestUniGraph {
  def main(args: Array[String]): Unit = {
    val adjList = Map(
      "A" -> List("B", "C"),
      "B" -> List("A", "C"),
      "C" -> List("A", "B", "D"),
      "D" -> List("C")
    )

    val graph1 = new UndirectedGraph(adjList)
    val graph2 = graph1.addEdge("A", "D")

    println(graph1.vertices)
    println(graph1.edges)
    println(graph1.neighbors("D"))

    println(graph2.vertices)
    println(graph2.edges)
    println(graph2.neighbors("D"))
  }
}
