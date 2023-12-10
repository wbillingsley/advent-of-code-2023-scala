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

@main def main() = 
    val lines = Source.fromFile("test2a.txt").getLines().toSeq

    val maxY = lines.indices.max
    val maxX = lines.head.indices.max

    val map:Map[Coord, Char] = 
        (for 
            y <- lines.indices 
            x <- lines(y).indices
        yield (x, y) -> lines(y)(x)).toMap

    val (sx, sy) = map.find({ case (c, ch) => ch == 'S' }).get._1


    val md = 
        map.map({ case (c, ch) => 
            if ch == 'S' then 
                // println(s"char at $c is ${map(c)}")
                // println(s"ffs ${for d <- all yield d}")
                val surroudingLocs = 
                    for 
                        d <- all if map.contains(c + d) 
                        toHere = {
                            val mm = moveDirections(map(c + d)) 
                            // println(s"Loc ${c + d} ch ${map(c + d)} dir $mm")
                            mm
                        } if toHere.contains(d.inverse)
                    yield d

                // println(s"From start, I can go $surroudingLocs")
                
                c -> surroudingLocs
            else c -> moveDirections.getOrElse(ch, Seq.empty)})
        


    println(s"Start location is $sx $sy")

    println(md((sx, sy)))

    val j = JellyFlood(md)(map)
    println("start forward flood")
    j.flood((sx, sy), 0)
    println("Done forward flood")

    val ((endX, endY), maxDistance) = j.maxDistance()
    println(s"Furthest distance is $maxDistance at $endX $endY")

    j.pp()

    // For part 2, we need to count crossings
    // First, though, we have to eliminate dead ends

    // Let's try something. Jelly flood in the reverse direction
    // Squares are only on the loop if the sum of their entries in both maps is the maximum distance.

    val reverseJ = JellyFlood(md)(map)
    println("start reverse")
    reverseJ.flood((endX, endY), 0)
    println("end reverse")
    println(reverseJ.maxDistance())

    reverseJ.pp()

    val bothDirections = 
        for (p, v) <- j.distance yield p -> (v + reverseJ.distance(p))


    // for (p, sum) <- bothDirections do println(s"$p $sum")
    
    // for (p, v) <- j.distance do
    //     val forward = v
    //     val back = reverseJ.distance(p)
    //     println(s"$p is distance ${forward} and ${back} .. ${forward + back}")

    val loopSquares = bothDirections.filter({ case (_, v) => v == maxDistance})

    println("---TRIMMED MAP---")

    def pp(locations:Set[Coord]):Unit = 
        for y <- 0 to maxY do
            for x <- 0 to maxX do
                if locations.contains((x, y)) then print(map((x, y))) else print(" ")
            println()


    pp(loopSquares.keySet.toSet)
    println()

    // Let's go back to whether the number of crossings is odd or even. But let's take into account whether you are travelling along a pipe.

    def canTraverse(pp:Coord, dir:Coord) = 
        val dest = pp + dir
        (!loopSquares.contains(pp) && !loopSquares.contains(dest)) || md.getOrElse(pp, Seq.empty).contains(dir)


    def countCrossings(p:Coord, dir:Coord):Int =
        var count = 0
        var cursor = p
        while map.contains(cursor) do 
            val dest = cursor + dir
            if !loopSquares.contains(cursor) && loopSquares.contains(dest) then 
                count = count + 1
            else if loopSquares.contains(cursor) && !loopSquares.contains(dest) then 
                count = count 
            else if !canTraverse(cursor, dir) then 
                count = count + 2

            cursor = cursor + dir

        count

   //println("Range method")
    // println(for x <- 13 to maxX if !canTraverse((x, 5), East) yield x)

    println(countCrossings((71, 71), North))
    println(countCrossings((71, 71), East))
    println(countCrossings((71, 71), South))
    println(countCrossings((71, 71), West))

    //def allOddCrossings(p:Coord) = all.forall({ (d) => countCrossings(p, d) % 2 == 1 })

    val allSquares = for 
        x <- 0 to maxX
        y <- 0 to maxY
    yield (x, y)

    val odds = for p <- allSquares if !loopSquares.contains(p) && countCrossings(p, East) % 2 != 0 yield p


    println("Odds " + odds.length)

    pp(odds.toSet)










    

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

