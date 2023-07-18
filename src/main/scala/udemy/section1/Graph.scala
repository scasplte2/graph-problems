package udemy.section1

trait Graph[V] {
  // return a list of all vertices in the graph
  def vertices: List[V]

  // return a list of all edges
  def edges: List[(V, V)]

  // add a new edge to the graph
  def addEdge(a: V, b: V): Graph[V]

  // return all vertices adjacent to a given vertex
  def neighbors(vertex: V): List[V]

}

object Graph {
  def directed[V](adjacencyList: Map[V, List[V]]): DirectedGraph[V] = new DirectedGraph[V](adjacencyList)

  def directed[V](): DirectedGraph[V] = new DirectedGraph[V](Map[V, List[V]]())
}
