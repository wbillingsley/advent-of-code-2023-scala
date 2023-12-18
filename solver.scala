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
def decompose(line:String) = 
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
    (hexDir, hexDistParsed.toInt, col)

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val instructions = lines.map(decompose)

    println(instructions(0))

    var cursor:Coord = (0, 0)
    var last:Option[Coord] = None
    // coordinate, edge-from, edge-to
    var map = mutable.Map.empty[(Coord), (Coord, Coord, String)]
    for 
        (dir, dist, col) <- instructions
        i <- 1 to dist
    do
        cursor = cursor + dir
        map(cursor) = (dir.inverse, (0, 0), col)
        for l <- last do 
            val (from, zero, col) = map(l)
            map(l) = (from, dir, col)
        last = Some(cursor)

    val minX = map.keySet.map(_._1).min
    val minY = map.keySet.map(_._2).min

    val maxX = map.keySet.map(_._1).max
    val maxY = map.keySet.map(_._2).max


    // count what's inside the loop
    val insideInclusive = for 
        y <- minY to maxY
        x <- minX to maxX
        northCrossingsGoingEast = (for xx <- x to maxX if map.get((xx, y)).exists({ 
            case (from, to, _) => from == North || to == North
        }) yield 1).sum if map.contains((x, y)) || northCrossingsGoingEast % 2 == 1
    yield (x, y)

    def pp() = 
        println(s"---($minX $minY) to ($maxX $maxY)")
        for 
            y <- minY to maxY
        do     
            for x <- minX to maxX do
                if map.contains((x, y)) then
                    val (from, to, col) = map((x, y))
                    print("#")
                else if insideInclusive.contains((x, y)) then 
                    print("o")
                else print(".")
            println()

        

    //pp()

    println("Result " + insideInclusive.length)



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

