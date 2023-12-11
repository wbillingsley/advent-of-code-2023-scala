// This is the solution for part 2
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star19/solver.scala
// (or select the "star20" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*

type Line = Seq[Int]

val integers = "(-?\\d+)".r
val decimals = "(-?\\d+)([.]\\d+)?".r

def allIntegersIn(s:String) = integers.findAllIn(s).map(_.toLong).toSeq
def allDecimalsIn(s:String) = decimals.findAllIn(s).map(_.toDouble).toSeq

type Coord = (Int, Int)

extension (c:Coord) {
    def +(c2:Coord) = 
        val (x, y) = c
        val (xx, yy) = c2
        (xx + x, yy + y)

    def inverse = (-c._1, -c._2)
}

val North = (0, -1)
val South = (0, 1)
val East = (1, 0)
val West = (-1, 0)
val all = Seq(North, South, East, West)

val moveDirections:Map[Char, Seq[Coord]] = Map(
    '|' -> Seq(North, South),
    '-' -> Seq(East, West),
    'L' -> Seq(North, East),
    'J' -> Seq(North, West),
    '7' -> Seq(South, West),
    'F' -> Seq(South, East),
    '.' -> Seq.empty,
)

// Here's one I made earlier
// This is adapted from the "check" code I use with students here: 
// https://github.com/theIntelligentBook/thinkingaboutprogramming/blob/master/src/main/scala/willtap/imperativeTopic/JellyFlood.scala
// except for AoC, it needed some fixing for efficiency
class JellyFlood(allowedDirections: Map[Coord, Seq[Coord]])(map: Map[Coord, Char]) {
    val distance = mutable.Map.empty[(Int, Int), Int]

    def maxX = distance.keys.map(_._1).max
    def maxY = distance.keys.map(_._1).max

    def flood(p:(Int, Int), dist:Int):Unit = {
        val q = mutable.Queue(p -> dist)

        while !q.isEmpty do
            val (loc, d) = q.dequeue()
            val add = check(loc, d)
            // println(add)
            val filtered = for (p, v) <- add if q.find({ case (pp, vv) => pp == p && vv <= v}).isEmpty yield p -> v
            q.enqueueAll(filtered)
    }

    final def check(p:(Int, Int), dist:Int):Seq[(Coord, Int)] = {
        // println(s"Checking $p $dist")
        distance(p) = dist
        val (x, y) = p

        for {
            (dx, dy) <- all if (
            distance.getOrElse(p + (dx, dy), Int.MaxValue) > dist + 1 && 
                map.contains(p + (dx, dy)) &&
                allowedDirections.getOrElse(p, Seq.empty).contains((dx, dy))
            )
        } yield (p + (dx, dy), dist + 1)
    }

    def maxDistance() = 
        distance.maxBy({ case ((x, y), d) => d })

    def pp() = {
        val mx = maxY
        val my = maxY
        for y <- 0 to my do
            for x <- 0 to mx do
                if distance.contains((x, y)) then print("*") else print(" ")
            println()
    }

}


extension (m:Seq[String]) {
    def directions(x:Int, y:Int) = 
        val c= m(y)(x)
        if c == 'S' then all else moveDirections.getOrElse(c, Seq.empty)
}

def inflate(s:Seq[String]):Seq[String] = 
    for 
        line <- s
        inflated <- (if !line.contains('#') then Seq(line, line) else Seq(line))
    yield inflated


def pp(s:Seq[String]):Unit = 
    for line <- s do println(line)


@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq

    pp(inflate(lines))



    

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

