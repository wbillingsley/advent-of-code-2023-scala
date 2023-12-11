// This is the solution for part 2
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star21/solver.scala
// (or select the "star21" branch from GitHub)

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

def inflateLine(s:Seq[String], l:String):String = {
    (for (c, x) <- l.zipWithIndex yield 
        if s.exists((ll) => ll(x) == '#') then "" + c else "*").mkString
}

def inflate(s:Seq[String]):Seq[String] = 
    for 
        line <- s
        inflated <- {
            val inflatedLine = inflateLine(s, line)
            (if !line.contains('#') then
                val stars = inflatedLine.map(_ => '*') 
                Seq(stars) 
            else Seq(inflatedLine))
        }
    yield inflated


def pp(s:Seq[String]):Unit = 
    for line <- s do println(line)


@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val inflated = inflate(lines)
    pp(inflated)

    val galaxies = for 
        (line, y) <- inflated.zipWithIndex
        (c, x) <- line.zipWithIndex if c == '#'
    yield (x, y) 

    println(galaxies)

    val pairs = for 
        g1 <- galaxies
        g2 <- galaxies if g1 != g2
    yield (g1, g2)


    println(s"${pairs.length} pairs")

    val allowedDirections = (for 
        (line, y) <- inflated.zipWithIndex
        (c, x) <- line.zipWithIndex 
    yield 
        (x, y) -> (for 
            dir <- all 
            p2 = (x, y) + dir if inflated.indices.contains(p2._2) && inflated(y).indices.contains(p2._1) 
        yield dir)
    ).toMap

    val costs = (for 
        (line, y) <- inflated.zipWithIndex
        (c, x) <- line.zipWithIndex 
    yield 
        (x, y) -> (
            for 
                dir <- allowedDirections((x, y)) 
                p2 = (x, y) + dir if allowedDirections.contains(p2) 
            yield 
                if inflated(p2._2)(p2._1) == '*' then dir -> 1L else dir -> 1L
        ).toMap
    ).toMap


    val paths = for (s, e) <- pairs if s != e yield {
        val y2 = Math.max(s._2, e._2)
        val y1 = Math.min(s._2, e._2)
        val x2 = Math.max(s._1, e._1)
        val x1 = Math.min(s._1, e._1)

        val str = (for x <- x1 until x2 yield inflated(y1)(x)) ++ (for y <- y1 until y2 yield inflated(y)(x2))

        val distance = str.foldLeft(0L) { case (t, ch) => ch match {
            case '*' => t + 1000000L
            case _ => t + 1
        }}
        distance
        
    }

    println(paths.sum / 2)



    

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

