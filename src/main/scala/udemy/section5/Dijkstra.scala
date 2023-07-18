package udemy.section5

import udemy.section1.{DirectedWeightedGraph, Graph, WeightedEdge}

import scala.collection.immutable.Map
import scala.util.Try

case class ShortStep[V](parents: Map[V, V], unprocessed: Set[V], dists: Map[V, Int]) {
  val extractMin: Option[(V, Int)] =
    Try(unprocessed.minBy(n => dists(n))).toOption
      .map(n => (n, dists(n)))
}

class Dijkstra(graph: DirectedWeightedGraph[String]) {
  val initDistances = graph.vertices.map(_ -> Int.MaxValue).toMap + ("Mumbai" -> 0)
  val result = shortestPath(ShortStep(Map(), graph.vertices.toSet, initDistances))
  println(result.dists)
  graph.vertices.foreach(v => println(extractShortestPaths(v, result.parents).reverse))

  def shortestPath(step: ShortStep[String]): ShortStep[String] = {
    step.extractMin.map { case (vertex, currentDist) =>
      val newDistances = graph.neighborsWithWeight(vertex).collect {
        case WeightedEdge(m, w) if step.dists.get(m).exists(_ > currentDist + w) => m -> (currentDist + w)
      }
      val newParents = newDistances.map { case (m, _) => m -> vertex }
      shortestPath(
        ShortStep(
          step.parents ++ newParents,
          step.unprocessed - vertex,
          step.dists ++ newDistances
        ))
    }.getOrElse(step)
  }

  def extractShortestPaths(vertex: String, parents: Map[String, String]): List[String] =
    parents.get(vertex).map(p => vertex +: extractShortestPaths(p, parents)).getOrElse(List(vertex))
}

object Dijkstra {
  def main(args: Array[String]): Unit = {
    val g = new DirectedWeightedGraph(Map(
      "Mumbai" -> List(
        WeightedEdge("Goa", 60),
        WeightedEdge("Colombo", 160),
        WeightedEdge("Dubai", 170),
        WeightedEdge("Jammu", 50),
        WeightedEdge("Bangkok", 250),
        WeightedEdge("Dhaka", 360)
      ),
      "Goa" -> List(
        WeightedEdge("Chennai", 20)
      ),
      "Chennai" -> List(
        WeightedEdge("Colombo", 40)
      ),
      "Colombo" -> List(
        WeightedEdge("Bangkok", 210)
      ),
      "Dubai" -> List(
        WeightedEdge("Singapore", 320)
      ),
      "Singapore" -> List(
        WeightedEdge("Bangkok", 210)
      ),
      "Bangkok" -> List(
        WeightedEdge("Singapore", 260),
        WeightedEdge("Dhaka", 90)
      ),
      "Dhaka" -> List(
        WeightedEdge("Bangkok", 140)
      ),
      "Jammu" -> List(
        WeightedEdge("Dubai", 110),
        WeightedEdge("Kathmandu", 190)
      ),
      "Kathmandu" -> List(
        WeightedEdge("Jammu", 160),
        WeightedEdge("Dhaka", 90)
      ),
    ))

    new Dijkstra(g)
  }
}
