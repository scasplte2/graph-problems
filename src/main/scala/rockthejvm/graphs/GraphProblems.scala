package rockthejvm.graphs

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.jdk.Accumulator

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  // number of outbound edges to a given vertex
  def outDegree[T](graph: Graph[T], vertex: T): Int =
    graph.getOrElse(vertex, Set()).size

  // number of inbound edges to a given vertex
  def inDegree[T](graph: Graph[T], vertex: T): Int =
    graph.values.count(_.contains(vertex))

  def testDegrees(): Unit = {
    println(outDegree(socialNetwork, "Alice")) // 3
    println(inDegree(socialNetwork, "David")) // 2
  }

//  testDegrees()

  /**
   * find if there is a path from Alice -> Mary
   * --- my attempt
   * Alice -> Bob -> ()
   * Alice -> Charlie -> David -> Bob -> ()
   * Alice -> Charlie -> David -> Mary      // there exists a path
   * --- seems like DFS with a check of the current node
   */

  def neighbors[T](graph: Graph[T], vertex: T): Set[T] = graph.getOrElse(vertex, Set())

  def dfs[T](graph: Graph[T], vertex: T, visited: Set[T] = Set[T]()): Set[T] =
    if (visited.contains(vertex)) visited
    else {
      neighbors(graph, vertex).foldLeft(visited + vertex) { case (accVisited, v) =>
        dfs(graph, v, accVisited)
      }
  }

//  println(dfs(socialNetwork, "Alice"))

//   determine if there is a path from start -> end in graph
  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    /*
     Alice -> Mary

     ipt([Alice], []) =
     ipt([Bob, Charlie, David], [Alice]) =
     ipt([Charlie, David], [Bob, Alice]) =
     ipt([David, David], [Charlie, Bob, Alice]) =
     ipt([David, Bob, Mary], [David, Charlie, Bob, Alice]) =
     ipt([Bob, Mary], [David, Charlie, Bob, Alice]) =
     ipt([Mary], [David, Charlie, Bob, Alice]) =
     = true

     N nodes, E edges
     Complexity: O(E)
    */

  @tailrec
  def isPathTailrec(remaining: List[T], visited: Set[T]): Boolean =
      if (remaining.isEmpty) false
      else {
        val vertex = remaining.head
        if (vertex == end) true
        else if (visited.contains(vertex)) isPathTailrec(remaining.tail, visited)
        else isPathTailrec(remaining.tail ++ graph(vertex), visited + vertex)
      }

    isPathTailrec(List(start), Set.empty)
    }

  def testPaths(): Unit = {
    println(isPath(socialNetwork, "Alice", "Mary")) // true
    println(isPath(socialNetwork, "Bob", "Mary")) // false
  }

//  testPaths()

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    /*
      Charlie -> Mary

      fpt([(Charlie, [Charlie])], []) =
        neighbors = [David]
        tuples = [(David, [David, Charlie])]

      fpt([(David, [David, Charlie])], [Charlie]) =
        neighbors = [Bob, Mary]
        tuples = [(Bob, [Bob, David, Charlie], (Mary [Mary, David, Charlie])]

      fpt([(Bob, [Bob, David, Charlie]), (Mary [Mary, David, Charlie])], [David, Charlie]) =
        neighbors = []
        tuples = []

      fpt([(Mary, [Mary, David, Charlie])], [David, Charlie, Bob]) =

      [Charlie, David, Mary]
     */

    @tailrec
    def findPathTailrec(remaining: List[(T, List[T])], visited: Set[T]): List[T] = {
      if (remaining.isEmpty) List()
      else {
        val (vertex, currentPath) = remaining.head
        if (vertex == end) currentPath.reverse
        else if (visited.contains(vertex)) findPathTailrec(remaining.tail, visited)
        else findPathTailrec(remaining.tail ++ graph(vertex).map(v => (v, v :: currentPath)), visited + vertex)
      }
    }

    findPathTailrec(graph(start).map(v => (v, v :: List(start))).toList, Set())
  }

  def findCycle[T](graph: Graph[T], node: T): List[T] = findPath(graph, node, node)

  def testFindPath(): Unit = {
    println(findPath(socialNetwork, "Charlie", "Mary"))
    println(findPath(socialNetwork, "Alice", "Mary"))
    println(findPath(socialNetwork, "Bob", "Mary"))
  }

//  testFindPath()

  def testCycles(): Unit = {
    println(findCycle(socialNetwork, "Alice")) // List
  }

//  testCycles()

  def makeUndirected[T](graph: Graph[T]): Graph[T] = {
    def addEdge(graph: Graph[T], from: T, to: T): Graph[T] = {
      if (!graph.contains(from)) graph + (from -> Set(to))
      else graph + (from -> (graph(from) + to))
    }

    @tailrec
    def addOpposingEdges(remaining: Set[T], accumulator: Graph[T]): Graph[T] = {
      if (remaining.isEmpty) accumulator
      else {
        val vertex = remaining.head
        val neighbors = graph(vertex)
        val newGraph = neighbors.foldLeft(accumulator) { case (intGraph, neighbor) =>
          addEdge(intGraph, neighbor, vertex)
        }
        addOpposingEdges(remaining.tail, newGraph)
      }
    }

    addOpposingEdges(graph.keySet, graph)
  }

  def testUndirected(): Unit = {
    val undirectedNetwork = makeUndirected(socialNetwork)
    println(undirectedNetwork("Bob"))
    println(undirectedNetwork("Alice"))
    println(undirectedNetwork("David"))
  }

  testUndirected()

}
