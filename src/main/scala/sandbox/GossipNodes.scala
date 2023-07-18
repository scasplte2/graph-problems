package sandbox

import scala.collection.mutable

//Implement a distributed gossip integer max function:
//Imagine we’re testing a large gossip algorithm locally that would be deployed to thousands of machines.
//Each machine carries a unique integer value it has been pre-assigned with
//Each machine can only send messages to a small subset of neighbors.
//Information about peers is pre-determined, i.e. node 1 has connections to node 5 and 8
//Machines can only communicate in fixed rounds of one set of peer messages per round
//Where maximum number of messages possible per round would be N*k (N nodes, k neighbors).
//Desired output -- each machine knows the max value of the network
//Determine and print number of rounds for convergence and total number of messages
//Individual nodes do not need to know total number of rounds, but the test should determine it.
//Overall goal: minimize total number of messages and rounds.
//
//need to find max value in network
//only talk to peers
//
//topology is made up by me

object GossipNodes {
  trait Node {
    var id: String
    var knownMax: Int
    var peers: Set[Node] = Set.empty
    var numMessages: Int = 0
    var view: mutable.Map[String, Int] = mutable.Map()

    def broadcast(): Unit = peers.foreach( p => {
      if (view(p.id) < knownMax) {
        send(p)
        numMessages = numMessages + 1
      }
    })

    def receive(id: String, value: Int): Unit = {
      view(id) = value
      if (knownMax < value) knownMax = value
    }

    def send(target: Node): Unit = target.receive(id, knownMax)

    def addPeers(newPeers: Set[Node]): Unit = {
      peers ++= newPeers
      view ++= peers.map(p => p.id -> 0)
    }
  }

  def makeNode(nodeId: String, value: Int): Node = new Node {
    override var id: String = nodeId
    override var knownMax: Int = value
  }
}

object Network {
  def main(args: Array[String]): Unit = {
    var pA = GossipNodes.makeNode("A", 2)
    var pB = GossipNodes.makeNode("B", 1)
    var pC = GossipNodes.makeNode("C", 5)
    var pD = GossipNodes.makeNode("D", 0)
    var pE = GossipNodes.makeNode("E", 10)
    var pF = GossipNodes.makeNode("F", 3)

    pA.addPeers(Set(pB))
    pB.addPeers(Set(pA, pC))
    pC.addPeers(Set(pB, pD))
    pD.addPeers(Set(pC, pE))
    pE.addPeers(Set(pD, pF))
    pF.addPeers(Set(pE))

    val network = List(pA, pB, pC, pD, pE, pF)
    var numRounds = 0
    while (network.map(_.knownMax).toSet.size > 1) {
      println(numRounds)
      println(network.map(_.knownMax).toSet)
      numRounds = numRounds + 1
      network.foreach(_.broadcast())
    }

    println(numRounds)
    println(network.map(p => (p.id, p.knownMax)))
    println(network.map(p => (p.id, p.numMessages)))
  }
}
