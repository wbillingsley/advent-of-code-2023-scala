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
def possibilities(s:Step, cost:Int, valid: Coord => Boolean, costAt: Coord => Int, _memo:Map[Step, Int], limit:Int):(Map[Step, Int], Seq[(Step, Int)]) = {

    def over(cost:Int) = cost >= limit
    var memo = _memo

    val (p, head, c) = s
    val poss = if s._3 == 0 then 
        for 
            d <- Seq(East, South)
            square <- 4 to 10
            dd = d * square 
            pp = p + dd if valid(pp)
            tripCost = (for i <- 1 to square yield costAt(p + (d * i))).sum 
            totalCost = cost + tripCost if !over(totalCost) && (!memo.contains(pp, d, square) || memo(pp, d, square) > totalCost)
        yield 
            memo = memo.updated((pp, d, square), totalCost)
            (pp, d, square) -> totalCost
    else if c < 4 then 
        for 
            s <- Seq((p + head, head, c + 1)) if valid(s._1) 
        yield s -> (cost + costAt(s._1))
    else //if c >= 10 then 
        for 
            d <- ninetyDegrees(head) 
            square <- 4 to 10
            dd = d * square 
            pp = p + dd if valid(pp)
            tripCost = (for i <- 1 to square yield costAt(p + (d * i))).sum 
            totalCost = cost + tripCost if !over(totalCost) && (!memo.contains(pp, d, square) || memo(pp, d, square) > totalCost)
        yield 
            memo = memo.updated((pp, d, square), totalCost)
            (pp, d, square) -> totalCost

    (memo, poss)
    // else Seq.empty
        // for 
        //     s <- (p + head, head, c + 1) +: (for d <- ninetyDegrees(head) yield (p + d, d, 1)) 
}


@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val puz:Seq[Seq[Int]] = lines.map { (l) => 
        for 
            c <- l.toSeq
        yield ("" + c).toInt
    }
    val maxY = puz.indices.max
    val maxX = puz(maxY).indices.max

    def puzContains(p:Coord):Boolean = 
        val (x, y) = p
        // x < 2 && y < 2 &&
        puz.indices.contains(y) && puz(y).indices.contains(x)

    def distAt(coord:Coord):Int = 
        val (x, y) = coord
        puz(y)(x)

    val paths:Seq[DistanceMemo] = Seq(((0, 0), (0,0), 0) -> 0)
    def traverse() = 
        var b = paths
        var memo = Map.empty[Step, Int]
        var limit = Int.MaxValue
        while !b.isEmpty do 
            // println(b)
            //b.foreach { (step, dist) => memo = memo.updated(step, dist) }

            b = (b.flatMap { (step, dist) => 
                val (newMemo, poss) = possibilities(step, dist, puzContains, distAt, memo, limit) 
                memo = newMemo

                for 
                    (next, nextD) <- poss // if puzContains(next._1) && (!memo.contains(next) || memo(next) > nextD)
                yield 
                    //memo = memo.updated(next, nextD)
                    if next._1 == (maxX, maxY) && nextD < limit then limit = nextD
                    (next, nextD)

            })
            println(s"Handling ${b.length} distinct paths")

        memo

    println(s"bottom right: $maxX $maxX")

    println("before")
    val memo = traverse()
    println(memo.size)
    val result = memo.filter({ (step, d) => step._1 == (maxX, maxY) && step._3 >= 4 && step._3 <= 10 }).minBy(_._2)
    println(s"Result $result")





    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

