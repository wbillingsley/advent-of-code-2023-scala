// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable


// class JellyFlood(allowedDirections: Map[Coord, Seq[Coord]])(map: Map[Coord, Char]) {
//     val distance = mutable.Map.empty[(Int, Int), Seq[Int]]

//     def maxX = distance.keys.map(_._1).max
//     def maxY = distance.keys.map(_._1).max

//     def flood(p:(Int, Int), dist:Int, limit:Int):Unit = {
//         val q = mutable.Queue(p -> dist)

//         while !q.isEmpty && dist < limit do
//             println(limit)
//             val (loc, d) = q.dequeue()
//             val add = check(loc, d)
//             // println(add)
//             val filtered = for (p, v) <- add if q.find({ case (pp, vv) => pp == p && vv <= v}).isEmpty yield p -> v
//             q.enqueueAll(filtered)
//     }

//     final def check(p:(Int, Int), dist:Int):Seq[(Coord, Int)] = {
//         // println(s"Checking $p $dist")
//         distance(p) = distance.getOrElse(p, Seq.empty) :+ dist
//         val (x, y) = p

//         for {
//             d <- allowedDirections(p)
//         } yield (p + d, dist + 1)
//     }

//     def maxDistance() = 
//         distance.maxBy({ case ((x, y), d) => d.max })

//     def pp() = {
//         val mx = maxY
//         val my = maxY
//         for y <- 0 to my do
//             for x <- 0 to mx do
//                 if map.get((x, y)).contains('#') then 
//                     print('#') 
//                 else
//                     if distance.contains((x, y)) then print(distance((x, y))) else print(" ")
//             println()
//     }

// }

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    
    val map:Map[Coord, Char] = (for 
        y <- lines.indices
        x <- lines(y).indices
    yield (x, y) -> lines(y)(x)).toMap

    val allowedDirections = (for 
        coord <- map.keySet
        dd = all.filter((d) => map.contains(coord + d) && !(map(coord + d) == '#'))
    yield coord -> dd).toMap

    val start = map.find((_, c) => c == 'S').map(_._1).toSet
    println(s"Start is $start")
    var cursor = start

    for i <- 0 until 64 do
        cursor = cursor.flatMap { (p) => (for d <- allowedDirections(p) yield p + d).toSet }
        println(cursor.size)


        // 
        // j.flood(start, 0, 6)

        // j.pp()

        // val canReach = j.distance.count((_, x) => x.contains(6))
        // println(s"Can reach $canReach")



    


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

