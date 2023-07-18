package udemy.section3

import scala.collection.immutable.Map
import udemy.section1.Graph

object GameGraph {
  val g: Graph[String] = Graph.directed(Map(
    "Logging" -> List("Game", "Networking"),
    "Networking" -> List("Game"),
    "Commons" -> List("Physics", "Math"),
    "Math" -> List("Physics", "Graphics", "AI Engine"),
    "Physics" -> List("Game"),
    "Graphics" -> List("Game"),
    "AI Engine" -> List("Game")
  ))

  def main(args: Array[String]): Unit = {
    println(TopologicalSort.topologicalDfsSort(g))
  }
}

case class DfsStep[V](visited: Set[V] = Set[V](), sort: List[V] = Nil)

object TopologicalSort {
  def topologicalDfsSort[V](graph: Graph[V]): List[V] = {
    def recDfsSort(node: V, step: DfsStep[V]): DfsStep[V] = {
      if (step.visited.contains(node)) step
      else {
        val preDfsStep = step.copy(visited = step.visited + node)
        val postDfsStep = graph.neighbors(node)
          .foldLeft(preDfsStep) { case (s, n) => recDfsSort(n, s) }
        postDfsStep.copy(sort = node +: postDfsStep.sort)
      }
    }

    graph.vertices.foldLeft(DfsStep[V]()) { case(step, n) => recDfsSort(n, step) }.sort
  }
}
