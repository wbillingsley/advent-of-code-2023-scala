// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star33/solver.scala
// (or select the "star33" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

// Parses a line of puzzle
def decompose1(line:String) = 
    val s"$dir $dist (#$col)" = line

    val parsedDir = dir match {
        case "R" => East
        case "U" => North
        case "D" => South
        case "L" => West
    }
    (parsedDir, dist.toInt, col)



// Parses a line of puzzle
def decompose2(line:String) = 
    val s"$dir $dist (#$col)" = line

    val hexDist = col.take(5)
    val hexDistParsed = Integer.parseInt(hexDist, 16)

    val hexDir = col.last match {
        case '0' => East
        case '1' => South
        case '2' => West
        case '3' => North
        case _ => 
            println(hexDist)
            throw new IllegalArgumentException("pop")
    }

    val parsedDir = dir match {
        case "R" => East
        case "U" => North
        case "D" => South
        case "L" => West
    }
    (hexDir, hexDistParsed.toInt, "#")


type Edge = (Coord, Coord, Coord, Coord) // (from, to, dir1, dir2). the last two aren't strictly necessary, but we might as well

extension (e:Edge) {
    def minX = Math.min(e._1._1, e._2._1)
    def minY = Math.min(e._1._2, e._2._2)
    def maxX = Math.max(e._1._1, e._2._1)
    def maxY = Math.max(e._1._2, e._2._2)

    def inclusiveLength = e.maxY - e.minY + e.maxX - e.minX + 1
    def exclusiveLength = e.maxY - e.minY + e.maxX - e.minX 

}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val instructions = lines.map(decompose1)


    // We need to keep a set of edges.
    var cursor:Coord = (0, 0)

    var edges = mutable.Set.empty[Edge]
    for 
        (dir, dist, col) <- instructions
    do
        val next = cursor + (dir * dist)
        println(s"From $cursor to $next in $dir")
        edges.add((cursor, next, dir, dir.inverse))
        cursor = next


    println(edges)

    val points = edges.map(_._1)

    val xAddresses = points.map(_._1).toSeq.sorted
    val yAddresses = points.map(_._2).toSeq.sorted

    val minX = xAddresses.min
    val maxX = xAddresses.max
    val minY = yAddresses.min
    val maxY = yAddresses.max

    println(s"---($minX $minY) to ($maxX $maxY), with ${edges.size} edges")

    // edges going south
    def sedgesAt(y:Int) = 
        edges.filter({ (e) => 
            e.maxY > y && e.minY <= y  
        }).toSeq.sortBy(_.minX)

    def hedgesAt(y:Int) = 
        edges.filter({ case (from, to, dir, idir) => 
            (
                (from._2 == y && to._2 == y)
            )            
        })


    def edgeLength = instructions.map(_._2).sum


    // There's about a million edges, but it's quick per edge
    var count = 0L
    
    val zippedYs = yAddresses.zip(yAddresses.tail) :+ (maxY, maxY + 1)

    val insides = for (y1, y2) <- zippedYs yield
        // sort the edges by their lowest x index
        val edges = sedgesAt(y1).toSeq.sortBy((e) => Math.min(e._1._1, e._2._1))

        def isHorizontal(e:Edge):Boolean = 
            val (from, to, dir, idir) = e
            idir == East || dir == East

        val horizontals = hedgesAt(y1)
        val verticals =  edges //edges.filter { case (from, to, dir, idir) => dir == North || idir == North }

        var parity = true
        val verticallyContainedRanges = 
            for 
                (e1, e2) <- (if verticals.nonEmpty then verticals.zip(verticals.tail) else Nil) if parity
            yield
                parity = !parity
                (e1.minX, e2.maxX)

        val enclosedHorizontals = 
            for 
                e <- horizontals 
                (r1, r2) <- verticallyContainedRanges if r1 <= e.minX && r2 >= e.maxX
            yield 
                Math.max(e.minX, r1 + 1) -> Math.min(e.maxX + 1, r2) 

        val doubleCounted = enclosedHorizontals.map((a, b) => b - a).sum


        val enclosedPerLine = verticallyContainedRanges.map((x1, x2) => x2 - x1 - 1).sum

        println(s"Verticles at $y1 to $y2 are $verticals enclosing $enclosedPerLine capturing $enclosedHorizontals")


        enclosedPerLine * (y2 - y1) - doubleCounted // + (unenclosedHorizontals.map(_.inclusiveLength).sum)


    println(s"Strictly inside is ${insides.sum} edge is $edgeLength  total is ${insides.sum + edgeLength}")

    pp()

    // // To find the north crossings efficiently, we need to find the edges
    // val insideInclusive = for 
    //     y <- minY to maxY
    //     x <- minX to maxX
    //     northCrossingsGoingEast = (for xx <- x to maxX if map.get((xx, y)).exists({ 
    //         case (from, to, _) => from == North || to == North
    //     }) yield 1).sum if map.contains((x, y)) || northCrossingsGoingEast % 2 == 1
    // yield (x, y)

    def pp() = 
        val edgePoints = mutable.Set.empty[Coord]
        for 
            e <- edges.toSeq
            x <- e.minX to e.maxX
            y <- e.minY to e.maxY
        do 
            edgePoints.add((x, y))

        println(s"---($minX $minY) to ($maxX $maxY)")
        for 
            y <- minY to maxY
        do     
            for x <- minX to maxX do
                if edgePoints.contains((x, y)) then
                    print("#")
                else print(".")
            println()

        

    //pp()

    // println("Result " + insideInclusive.length)



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

