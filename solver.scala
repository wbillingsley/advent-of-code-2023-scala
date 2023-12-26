// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star50/solver.scala
// (or select the "star50" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable
import java.{util => ju}
import scala.util.Random

type Connection = (String, String)

extension (c:Connection) {
    def a = c._1
    def b = c._2
    def nodes = Seq(c.a, c.b)
}

case class ConnectionSet(s:Set[Connection]) {

    lazy val leftNodeMap = s.groupBy(_.a).toMap
    lazy val rightNodeMap = s.groupBy(_.b).toMap

    lazy val nodes = leftNodeMap.keySet ++ rightNodeMap.keySet

    def connectionsFor(node:String):Set[Connection] = leftNodeMap.getOrElse(node, Set.empty) ++ rightNodeMap.getOrElse(node, Set.empty)

    def removed(c:Connection):ConnectionSet = 
        val reverse = (c._2, c._1)
        ConnectionSet((s - c) - reverse)

    @tailrec
    final def reachableFrom(a:Set[String], visited:Set[String] = Set.empty):Set[String] = {
        if a.isEmpty then visited else
            val next = for 
                n <- a
                c <- connectionsFor(n)
                node <- c.nodes if node != n && !visited.contains(node)
            yield node
            reachableFrom(next, visited ++ a)
    }

    @tailrec
    final def bfsUntil[T](a:Set[String], visited:Set[String] = Set.empty)(effect: Set[String] => Option[T]):Option[T] = {
        if a.isEmpty then None else
            val next = for 
                n <- a
                c <- connectionsFor(n)
                node <- c.nodes if !a.contains(node) && !visited.contains(node)
            yield node
            val r = effect(next)
            if r.nonEmpty then r else bfsUntil(next, visited ++ a)(effect) 
    }

    def nodePathFromTo(from:String, to:String):Option[List[String]] = 
        var sets:List[Set[String]] = List(Set(from))
        val search = bfsUntil(Set(from)) { (set) => 
            sets = set :: sets
            if set.contains(to) then Some(sets) else None
        }

        search match {
            case None => None
            case Some(list) => 
                // println(s"Contains : " + list.head.contains(to))
                Some(list.tail.foldLeft(List(to)) { case (path, set) => 
                    // println(s" $path $set")
                    val poss = connectionsFor(path.head).flatMap(_.nodes)
                    set.find((n) => poss.contains(n)) match {
                        case Some(n) => n :: path
                        case None =>
                            println(s"From: $from to $to")
                            println(s"Path: $path")
                            println(s"Set: $set")
                            println(s"Poss: $poss")
                            println(s"List: $list")
                            throw ju.NoSuchElementException()
                    }
                })
        }

    def connPathFromTo(from:String, to:String):Option[List[Connection]] = 
        for np <- nodePathFromTo(from, to) yield 
            np.zip(np.tail).map { (a, b) => (a, b) }


    def isBridge(c:Connection):Boolean =         
        !removed(c).reachableFrom(Set(c.a)).contains(c.b)

    def bridges = s.filter(isBridge(_))

    def bridgeCount = bridges.size

}

def decompose(line:String):Set[Connection] = {
    val s"$from: $toList" = line
    val to = toList.split(' ').map(_.trim).toSeq
    (for b <- to yield from -> b).toSet
}



@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    
    val cs = ConnectionSet(lines.map(decompose).reduce(_ ++ _))

    // So, first I misread the problem and thought we were splitting it into *three*
    // (In which case, removing one of the edges would leave two bridges.)
    // But no, we're only splitting it in two.
    // println(set.find((c) => connections.removed(c).bridges.nonEmpty))

    def bottleneck(connections:ConnectionSet):Connection = {

        // If we walk from some nodes to their most distant nodes, we ought to pass through the "neck" most
        def longestPath(santa:String):List[Connection] = {
            var step = 1
            val distMap = mutable.Map.empty[String, Int]
            connections.bfsUntil(Set(santa)) { (set) => 
                for n <- set do 
                    distMap(n) = step
                step = step + 1
                None
            }

            val furthest = distMap.maxBy(_._2)._1
            connections.connPathFromTo(santa, furthest).get
        }

        val connList = for 
            i <- 0 until 70
            santa = connections.s.toSeq(Random.nextInt(connections.s.size)).a
            lp = longestPath(santa)
            conn <- lp
        yield conn

        val counts = connList.groupBy(identity).map({ (c, cc) => (c, cc.length)})


        counts.maxBy(_._2)._1

    }

    var cursor = cs
    for i <- 1 to 3 do
        val b = bottleneck(cursor)
        cursor = cursor.removed(b)
        println(s"$i : $b")

    // A few runs gives us different combinations

    // Time to verify
    val split = cs.removed("zcj" -> "rtt").removed("txl" -> "hxq").removed("gxv" -> "tpn")
    val canSpan = split.nodePathFromTo("zcj", "rtt")
    println(s"Still reachable is $canSpan")

    // And by changing the list based on what we saw on the path, we can eventually find the three manually-iteratively. (Edit program and repeat)
    // 1 : (zcj,rtt)
    // 2 : (txl,hxq)
    // 3 : (gxv,tpn)

    println(s"Total size was ${cs.nodes.size}")

    val left = split.reachableFrom(Set("zcj"))
    val right = split.reachableFrom(Set("rtt"))

    println(s"Left is ${left.size} right is ${right.size}, mult is ${left.size * right.size}, sanity check total is ${left.size + right.size}")


    println("mapped")
    // val pairs = connections.nodes.toSeq.combinations(2).take(100)

    // val commonNodes = (for 
    //     Seq(a, b) <- pairs
    //     p <- connections.nodePathFromTo(a, b).toSeq
    //     node <- p
    // yield node).toSeq.groupBy(identity).map({ (n, nn) => n -> nn.length })

    // val tryThis = commonNodes.toSeq.sortBy(-_._2).take(3)
    // println(tryThis)

    // val test = Set("a" -> "b", "b" -> "d", "a" -> "c", "a" -> "d", "x" -> "y", "x" -> "z", "c" -> "x")
    // println(s"Test: " + ConnectionSet(test).isBridge("a" -> "b"))



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

