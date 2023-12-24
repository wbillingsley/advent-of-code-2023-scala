// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star45/solver.scala
// (or select the "star45" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable

type Coord3 = (Long, Long, Long)

type Hailstone = (Coord3, Coord3)

extension (h:Hailstone) {

    def x = h._1._1
    def y = h._1._2
    def z = h._1._3

    def vx = h._2._1
    def vy = h._2._2
    def vz = h._2._3

    def yGradient:Double = 
        val ((x, y, z), (vx, vy, vz)) = h
        vy.toDouble / vx

    // x value where it crosses the y axis
    def yCrossing:Double =
        val ((x, y, z), (vx, vy, vz)) = h
        x - y / yGradient

    def y0 = h.yAt(0)

    def isFutureX(x:Double) = 
        val dir = (Math.abs(h.vx) / h.vx).toInt
        val dir2 = (Math.abs(x - h.x) / (x - h.x)).toInt
        dir == dir2

    def yAt(x:Double) = 
        (x - h.yCrossing) * h.yGradient

    def xyIntersect(h2:Hailstone) :Option[(Double, Double)] = 
        if h.yParallel(h2) then None else
            val ydiff = h.y0 - h2.y0
            val x = ydiff / (h2.yGradient - h.yGradient)
            val y = h.yGradient * (x - h.yCrossing)
            Some((x, y))


    def yParallel(h2:Hailstone) = 
        h.yGradient == h2.yGradient

    // def yCrossing(h2:Hailstone):Option[(Double, Double)] = 
    //     if h.yParallel(h2) then None else
    //         val gDiff = h.yGradient - h2.yGradient
    //         // 
    //         val y0 = h2.y (h.x - h2.x) * gDiff

}

def decompose(line:String):Hailstone = {
    val s"$x, $y, $z @ $vx, $vy, $vz" = line
    val p = (x.trim.toLong, y.trim.toLong, z.trim.toLong)
    val v = (vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
    (p, v)
}

// Solves for t when x coordinates cross. Unfortunately, RTQ, that's not what we're doing
def xCrossingT(h1:Hailstone, h2:Hailstone) = {
    // x1 + tVx1 = x2 + tVx2
    // x1 - x2 = tVx2 - tVx1
    // x1 - x2 = t(Vx2 - Vx1)
    // t = (x1 - x2) / (Vx2 - Vx1)

    (h1._1._1 - h2._1._1).toDouble / (h2._2._1 - h1._2._1).toDouble    
}



@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val hailstones = lines.map(decompose)

    val pairs = hailstones.combinations(2)

    // val (low, high) = (7L, 27L)
    val (low, high) = (200000000000000L, 400000000000000L)
    
    val result = pairs.count({ case Seq(a, b) => 
        a.xyIntersect(b).exists((xx, yy) => 

            val r = low <= xx && xx <= high && low <= yy && yy <= high && a.isFutureX(xx) && b.isFutureX(xx)

            println(s"$a $b  -- $xx $yy -- $r")
            r

        )
    })

    println(s"Result is $result")



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

