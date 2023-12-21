// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable

// replicate a grid nine times
def nonicate[T](map:Map[Coord, T]):Map[Coord, T] = {
    val minX = map.keys.map(_._1).min
    val minY = map.keys.map(_._1).min
    val maxX = map.keys.map(_._1).max
    val maxY = map.keys.map(_._1).max
    val rangeX = maxX - minX
    val rangeY = maxY - minY

    (for 
        dx <- Seq(-rangeX, 0, rangeX)
        dy <- Seq(-rangeY, 0, rangeY)
        (x, y) <- map.keySet.toSeq
    yield
        (x + dx, y + dy) -> map((x, y))).toMap

}

class JellyFlood(allowedDirections: Map[Coord, Seq[Coord]])(map: Map[Coord, Char]) {
    val distance = mutable.Map.empty[(Int, Int), Int]

    def maxX = distance.keys.map(_._1).max
    def maxY = distance.keys.map(_._1).max

    def flood(p:(Int, Int), dist:Int)(limit:Int):Unit = {
        val q = mutable.Queue(p -> dist)

        while !q.isEmpty do
            val (loc, d) = q.dequeue()
            val add = check(loc, d)
            // println(add)
            val filtered = for (p, v) <- add if q.find({ case (pp, vv) => pp == p && vv <= v}).isEmpty && v <= limit yield p -> v
            q.enqueueAll(filtered)
    }

    final def check(p:(Int, Int), dist:Int):Seq[(Coord, Int)] = {
        // println(s"Checking $p $dist")
        distance(p) = dist //distance.getOrElse(p, Seq.empty) :+ dist
        val (x, y) = p

        for {
            d <- allowedDirections(p) if !distance.get(p + d).exists(_ <= dist + 1)
        } yield (p + d, dist + 1)
    }

    def maxDistance() = 
        distance.maxBy({ case ((x, y), d) => d })

    def pp() = {
        val mx = maxY
        val my = maxY
        for y <- 0 to my do
            for x <- 0 to mx do
                if map.get((x, y)).contains('#') then 
                    print('#') 
                else
                    if distance.contains((x, y)) then print(distance((x, y))) else print(" ")
            println()
    }

}

@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq

    val map:Map[Coord, Char] = (for 
        y <- lines.indices
        x <- lines(y).indices
    yield (x, y) -> lines(y)(x)).toMap

    val allowedDirections = (for 
        coord <- map.keySet
        dd = all.filter((d) => map.contains(coord + d) && !(map(coord + d) == '#'))
    yield coord -> dd).toMap

    val start = map.find((_, c) => c == 'S').map(_._1).get
    println(s"Start is $start")
    var cursor = start

    // for i <- 0 until 64 do
    //     cursor = cursor.flatMap { (p) => (for d <- allowedDirections(p) yield p + d).toSet }
    //     println(cursor.size)



    val j = JellyFlood(allowedDirections)(map)
    j.flood(start, 0)(9)//(Int.MaxValue)

    j.pp()

    val canReach = j.distance.count((_, x) => (x - 6) >= 0 && (x - 6) % 2 == 0)
    println(s"Can reach $canReach")



    


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

