// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star33/solver.scala
// (or select the "star33" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

type Line = Seq[Int]

val integers = "(-?\\d+)".r
val decimals = "(-?\\d+)([.]\\d+)?".r

def allIntegersIn(s:String) = integers.findAllIn(s).map(_.toLong).toSeq
def allDecimalsIn(s:String) = decimals.findAllIn(s).map(_.toDouble).toSeq

// location, heading (where from), step count
type Step = (Coord, Coord, Int)
type Path = Seq[Step]

type DistanceMemo = (Step, Int)

// perpendicular to current heading
def ninetyDegrees(heading:Coord):Seq[Coord] = heading match {
    case North | South => Seq(East, West)
    case East | West => Seq(North, South)
    case _ => Seq(North, East, South, West) // starting case
}

// allowable moves
def possibilities(s:Step):Seq[Step] = {
    val (p, head, c) = s
    if s._2 == (0,0) then Seq((p + North, North, 1), (p + East, East, 1), (p + South, South, 1), (p + West, West, 1)) // starting case
    else if c < 4 then 
        Seq((p + head, head, c + 1))
    else if c >= 10 then 
        for d <- ninetyDegrees(head) yield (p + d, d, 1)
    else (p + head, head, c + 1) +: (for d <- ninetyDegrees(head) yield (p + d, d, 1)) 
}


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

@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq

    val instructions = lines.map(decompose1)

    println(instructions(0))

    var cursor:Coord = (0, 0)
    // coordinate, edge-from, edge-to

    // We need to keep a set of edges.

    println((6, 0) + ((0, 1) * 5))

    type Edge = (Coord, Coord, Coord, Coord) // (from, to, dir1, dir2). the last two aren't strictly necessary, but we might as well
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

    val maxX = xAddresses.max
    val maxY = xAddresses.max
    val minX = yAddresses.min
    val minY = yAddresses.min

    println(s"---($minX $minY) to ($maxX $maxY), with ${edges.size} edges")

    // Only get edges we're strictly within. We'll add the edges separately
    def northEdges(y:Int) = 
        edges.filter({ case (from, to, dir, idir) => 

            (dir == North) &&             // going north
            (
                (from._2 <= y && to._2 >= y) ||      // contains y
                (to._2 <= y && from._2 >= y)
            )
            
        })

    def edgeLength = instructions.map(_._2).sum

    println(edgeLength)


    // There's about a million edges, but it's quick per edge
    var count = 0L
    
    val zippedYs = yAddresses.zip(yAddresses.tail)

    val insides = for (y1, y2) <- zippedYs yield
        val edges = northEdges(y2) 

        println(s"Edges from $y1 to $y2 are $edges")


        val edgeXs = edges.toSeq.map(_._1._1).sorted // sort edges based on x position; we no longer need y
        val zipped = if edgeXs.nonEmpty then edgeXs.zip(edgeXs.tail) else Nil
        var parity = false
        val covered = zipped.foldLeft(0) { case (tot, (a, b)) => 
            parity = !parity
            if parity then tot + b - a else tot
        }

        covered.toLong * (y2 - y1 + 1)


    println(insides.sum + edgeLength)


    // // To find the north crossings efficiently, we need to find the edges
    // val insideInclusive = for 
    //     y <- minY to maxY
    //     x <- minX to maxX
    //     northCrossingsGoingEast = (for xx <- x to maxX if map.get((xx, y)).exists({ 
    //         case (from, to, _) => from == North || to == North
    //     }) yield 1).sum if map.contains((x, y)) || northCrossingsGoingEast % 2 == 1
    // yield (x, y)

    // def pp() = 
    //     println(s"---($minX $minY) to ($maxX $maxY)")
    //     for 
    //         y <- minY to maxY
    //     do     
    //         for x <- minX to maxX do
    //             if map.contains((x, y)) then
    //                 val (from, to, col) = map((x, y))
    //                 print("#")
    //             else if insideInclusive.contains((x, y)) then 
    //                 print("o")
    //             else print(".")
    //         println()

        

    //pp()

    // println("Result " + insideInclusive.length)



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

