package sandbox

import scala.annotation.tailrec

object ConnectedComponents extends App {

  // 0, 1, 5, 50, 15, 20
  val graph: Map[Int, Set[Int]] = Map(
    (0 -> Set(1, 5)),
    (1 -> Set(0)),
    (5 -> Set(0, 50)),
    (50 -> Set(5)),
    (15 -> Set(20)),
    (20 -> Set(15))
  )

  // second attempt
  // traverse and collect
  // collectComponents([15,0,1,5,50,20] [[]])
  //   traverse([15], [])
  //   traverse([20], [15])
  //   traverse([], [20,15])
  //   -> [20,15]
  // collectComponents([0,1,5,50], [[20,15]]
  //   traverse([0], [], [])
  //   traverse([1,5], [0])
  //   traverse([5], [0,1])
  //   traverse([50], [5,0,1])
  //   traverse([], [50,5,0,1])
  //   -> [50,5,0,1]
  // collectComponents([], [[50,5,0,1], [20,15]])
  // -> [[50,5,0,1], [20,15]]
  def findConnected_v2(graph: Map[Int, Set[Int]]): List[List[Int]] = {
    @tailrec
    def traverseGraph(remaining: List[Int], visited: Set[Int]): Set[Int] = {
      if (remaining.isEmpty) visited
      else {
        val node = remaining.head

        if (visited.contains(remaining.head)) traverseGraph(remaining.tail, visited)
        else traverseGraph(graph(node).toList ++ remaining.tail, visited + node)
      }
    }

    @tailrec
    def collectComponents(remaining: List[Int], result: List[List[Int]]): List[List[Int]] = {
      if (remaining.isEmpty) result
      else if (result.flatten.contains(remaining.head)) collectComponents(remaining.tail, result)
      else {
        val subComponent = traverseGraph(List(remaining.head), Set())
        collectComponents(remaining.tail, subComponent.toList :: result)
      }
    }

    collectComponents(graph.keys.toList, List())
  }




  // interview attempt
  // consider all keys as starting
  // [1,5,50,15,20] [0] [[]] []
  // [1,5,50,15,20] [5,1,0] [[]] [0]
  // [1,5,50,15,20] [50,5,1,0] [[]] [5,1,0]
  // [1,5,50,15,20] [[50,5,1,0]], [50,5,1,0] // since in buffer and visited move to remaining tail
  // [5,50,15,20] [[50,5,1,0]], [50,5,1,0] //since 1 is in visited, don't start another list in acc
  // .. same for 5 & 50
  // [15,20] [[50,5,1,0]] [50,5,1,0] // since 15 not in visited push to acc as new list
  // [20] [[15] [50,5,1,0]] [50,5,1,0] // expand 15
  // [20] [[20,15], [50,5,1,0]] [15,50,5,1,0] //expand 20
  // [20] [[20,15], [50,5,1,0]] [20,15,50,5,1,0] // 20 in acc and visited move to remaining tail
  // [] [[20,15], [50,5,1,0]] [20,15,5,1,0] // remaining is empty, return acc
  // [[20,15], [50,5,1,0]]

  def findConnected(graph: Map[Int, Set[Int]]): List[List[Int]] = {
    @tailrec
    def findConnectedTailrec(remaining: List[Int], buffer: Set[Int], acc: List[List[Int]], visited: Set[Int]): List[List[Int]] = {
      if (remaining.isEmpty) acc
      else if (acc.forall(comp => comp.contains(remaining.head)) || buffer.contains(remaining.head)) {
        findConnectedTailrec(remaining.tail, Set(remaining.head), acc, visited)
      }
      else if (!visited.contains(buffer.head)) {
        findConnectedTailrec(remaining, graph(buffer.head) ++ buffer, acc, visited + buffer.head)
      } else {
        findConnectedTailrec(remaining, Set(), buffer.toList :: acc, visited)
      }
    }

    val nodes = graph.keySet
    findConnectedTailrec(nodes.toList, Set(), List(), Set())
  }

  // println(graph)
  // println(List(List(1,2,3,0), List(2,3)).exists(comp => comp.contains(0)))
  println(findConnected_v2(graph))

}
