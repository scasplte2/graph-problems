package udemy.section1

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import scala.collection.immutable.Map

class DirectedGraphSpec extends AnyPropSpec with Matchers {

  property("DirectedGraph should be able to add an edge") {
    val graph1 = new DirectedGraph(Map(
      "A" -> List("B", "C"),
      "B" -> List("C"),
      "C" -> List("D"),
      "D" -> Nil
    ))

    val graph2 = graph1.addEdge("E", "B")

    def checkEdges(a: String, b: String, graph: DirectedGraph[String]): Boolean =
      graph.edges.contains((a, b))

    checkEdges("E", "B", graph1) shouldBe false
    checkEdges("E", "B", graph2) shouldBe true
  }
}
