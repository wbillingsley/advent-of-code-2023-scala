// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star20/solver.scala
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
class JellyFlood(allowedDirections: Map[Coord, Seq[Coord]]) {
    val distance = mutable.Map.empty[(Int, Int), Int]


    def flood(p:(Int, Int), dist:Int):Unit = {
        val q = mutable.Queue(p -> dist)

        while !q.isEmpty do
            val (loc, d) = q.dequeue()
            val add = check(loc, d)
            println(add)
            q.enqueueAll(add)
    }

    final def check(p:(Int, Int), dist:Int):Seq[(Coord, Int)] = {
        println(s"Checking $p $dist")
        distance(p) = dist
        val (x, y) = p

        for {
            (dx, dy) <- all if (
            distance.getOrElse(p + (dx, dy), Int.MaxValue) > dist + 1 &&
                allowedDirections.getOrElse(p, Seq.empty).contains((dx, dy))
            )
        } yield (p + (dx, dy), dist + 1)
    }

    def maxDistance() = 
        distance.maxBy({ case ((x, y), d) => d })

}


extension (m:Seq[String]) {
    def directions(x:Int, y:Int) = 
        val c= m(y)(x)
        if c == 'S' then all else moveDirections.getOrElse(c, Seq.empty)
}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val map:Map[Coord, Char] = 
        (for 
            y <- lines.indices 
            x <- lines.indices
        yield (x, y) -> lines(y)(x)).toMap

    val (sx, sy) = map.find({ case (c, ch) => ch == 'S' }).get._1


    val md = 
        map.map({ case (c, ch) => 
            if ch == 'S' then 
                println(s"char at $c is ${map(c)}")
                println(s"ffs ${for d <- all yield d}")
                val surroudingLocs = 
                    for 
                        d <- all if map.contains(c + d) 
                        toHere = {
                            val mm = moveDirections(map(c + d)) 
                            println(s"Loc ${c + d} ch ${map(c + d)} dir $mm")
                            mm
                        } if toHere.contains(d.inverse)
                    yield d

                println(s"From start, I can go $surroudingLocs")
                
                c -> surroudingLocs
            else c -> moveDirections.getOrElse(ch, Seq.empty)})
        


    println(s"Start is $sx $sy")

    println(md((sx, sy)))

    val j = JellyFlood(md)
    println("start")
    j.flood((sx, sy), 0)
    println("Done")

    println(j.maxDistance())

    

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

