package udemy.section4

import udemy.section1.Graph

import scala.collection.immutable.Map

case class DfsCycleResult[V](visited: List[V], isCyclic: Boolean = false)

object CycleDetection {

  def checkForCycle[V](vertex: V,
                       graph: Graph[V],
                       visited: List[V] = List[V](),
                       ancestors: List[V] = List[V]()
                      ): DfsCycleResult[V] = {
    if (ancestors.contains(vertex)) DfsCycleResult(visited, isCyclic = true)
    else if (visited.contains(vertex)) DfsCycleResult(visited)
    else {
      graph.neighbors(vertex).foldLeft(DfsCycleResult(visited :+ vertex)) {
        case (DfsCycleResult(_visited, true), _) => DfsCycleResult(_visited, isCyclic = true)
        case (DfsCycleResult(_visited, false), _vertex) => checkForCycle(_vertex, graph, _visited, ancestors :+ vertex)
      }
    }
  }

  def containsCycle[V](graph: Graph[V]): Boolean = {
    val startNodes = graph.vertices.filter(v => graph.edges.forall(e => e._2 != v))
    startNodes.isEmpty || startNodes.exists(checkForCycle(_, graph).isCyclic)
  }

  def main(args: Array[String]): Unit = {
    val g: Graph[String] = Graph.directed(Map(
      "A" -> List("B", "C", "D"),
      "B" -> List("D"),
      "C" -> List("D"),
      "D" -> List("C"),
      "E" -> List("C")
    ))

    println(containsCycle(g))
  }

}
