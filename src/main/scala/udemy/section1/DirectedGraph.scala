package udemy.section1

import scala.collection.immutable.Map

// Directed implementation of the Graph data structure
class DirectedGraph[V](adjacencyList: Map[V, List[V]]) extends Graph[V] {
  override def vertices: List[V] = adjacencyList.keys.toList

  override def edges: List[(V, V)] = adjacencyList.foldLeft(List[(V,V)]()) { (edgeAcc, adj) =>
    edgeAcc ++ adj._2.map(x => (adj._1, x))
  }

  override def addEdge(a: V, b: V): DirectedGraph[V] = {
    val aNeighbors = adjacencyList.getOrElse(a, Nil)
    if (aNeighbors.contains(b)) new DirectedGraph(adjacencyList)
    else new DirectedGraph(adjacencyList + (a -> (aNeighbors :+ b)))
  }

  override def neighbors(vertex: V): List[V] = adjacencyList.getOrElse(vertex, Nil)
}

object TestDiGraph {
  def main(args: Array[String]): Unit = {
    val adjList = Map(
      "A" -> List("B", "C"),
      "B" -> List("C"),
      "C" -> List("D"),
      "D" -> Nil
    )

    val graph1 = new DirectedGraph(adjList)
    val graph2 = graph1.addEdge("E", "B")
    val graph3 = graph1.addEdge("A", "B")
    val graph4 = graph1.addEdge("D", "A")

    println(graph1.vertices)
    println(graph1.edges)
    println(graph1.neighbors("A"))

    println(graph2.vertices)
    println(graph2.edges)

    println(graph3.vertices)
    println(graph3.edges)

    println(graph4.vertices)
    println(graph4.edges)
  }
}
