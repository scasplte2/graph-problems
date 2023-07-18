package udemy.section2

import udemy.section1.{DirectedGraph, Graph}

import scala.collection.immutable.{Map, Queue}

object Traversal {
  def traversalDFS[V](start: V, graph: Graph[V])(process: V => Unit, visited: Set[V]): Set[V] = {
    if (visited.contains(start)) visited
    else {
      process(start)
      graph.neighbors(start).foldLeft(visited + start) { case (allVisited, n) =>
        traversalDFS(n, graph)(process, allVisited)
      }
    }
  }

  def traversalDFS_iter[V](start: V, graph: Graph[V], f: V => Unit): Unit = {
    LazyList.iterate((List(start), Set[V](start))) { case (stack, visited) =>
      val vertex = stack.head
      val newStack = graph.neighbors(vertex).filterNot(visited.contains) ++ stack.tail
      val newVisited = graph.neighbors(vertex).toSet ++ visited
      (newStack, newVisited)
    }
      .takeWhile(t => t._1.nonEmpty).foreach(t => f(t._1.head))
  }

  def traversalBFS[V](start: V, graph: Graph[V], f: V => Unit): Unit = {
    LazyList.iterate((Queue(start), Set[V](start))) { case (queue, visited) =>
      val (vertex, rest) = queue.dequeue
      val newQueue = rest.enqueueAll(graph.neighbors(vertex).filterNot(visited.contains))
      val newVisited = graph.neighbors(vertex).toSet ++ visited
      (newQueue, newVisited)
    }
      .takeWhile(t => t._1.nonEmpty).foreach(t => f(t._1.head))
  }
}

object DepthFirstSearch {
  // doesn't work if there are cycles
  def traverse[V](g: Graph[V], vertex: V): List[V] =
    g.neighbors(vertex).foldLeft(List(vertex)){ case (acc, v) => acc ++ traverse(g, v)}

  def traverse_v2[V](g: Graph[V], vertex: V, visited: Set[V] = Set[V]()): Set[V] = {
    if (visited.contains(vertex)) visited
    else {
      g.neighbors(vertex).foldLeft(visited + vertex) { case (accVisited, v) =>
        traverse_v2(g, v, accVisited)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val adjList = Map(
      "A" -> List("C"),
      "B" -> List("C", "A"),
      "C" -> List("D"),
      "D" -> Nil
    )

    val adjList_2 = Map(
      "A" -> List("G", "B"),
      "B" -> List("C"),
      "C" -> List("D", "E"),
      "D" -> List("E"),
      "G" -> List("H"),
      "H" -> List("F"),
      "F" -> List("A")
    )



    println("------- v1")
    println(traverse(new DirectedGraph(adjList), "B"))
    println("------- v2")
    println(traverse_v2(new DirectedGraph(adjList), "B"))
    println("------- traversalDFS_iter on graph 1")
    Traversal.traversalDFS_iter("B", new DirectedGraph(adjList), println)
    println("------- traversalDFS_iter on graph 2")
    Traversal.traversalDFS_iter("A", new DirectedGraph(adjList_2), println)
    println("------- traversalBFS")
    Traversal.traversalBFS("A", new DirectedGraph(adjList_2), println)
  }
}
