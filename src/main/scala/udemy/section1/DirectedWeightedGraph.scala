package udemy.section1

case class WeightedEdge[V](destination: V, weight: Int)

class DirectedWeightedGraph[V](adjacencyList: Map[V, List[WeightedEdge[V]]]) extends Graph[V] {
  override def vertices: List[V] = adjacencyList.keys.toList

  override def edges: List[(V, V)] = adjacencyList.foldLeft(List[(V,V)]()) { case (edgeAcc, (vertex, neighbors)) =>
    edgeAcc ++ neighbors.map(n => (vertex, n.destination))
  }

  def addEdge(a: V, b: V, weight: Int): DirectedWeightedGraph[V] = {
    val aNeighbors = adjacencyList.getOrElse(a, Nil)
    if (aNeighbors.contains(b)) new DirectedWeightedGraph(adjacencyList)
    else new DirectedWeightedGraph(adjacencyList + (a -> (aNeighbors :+ WeightedEdge(b, weight))))
  }

  override def addEdge(a: V, b: V): DirectedWeightedGraph[V] = addEdge(a, b, 0)

  override def neighbors(vertex: V): List[V] = adjacencyList.getOrElse(vertex, Nil).map(_.destination)

  def neighborsWithWeight(vertex: V): List[WeightedEdge[V]] = adjacencyList.getOrElse(vertex, Nil)
}
