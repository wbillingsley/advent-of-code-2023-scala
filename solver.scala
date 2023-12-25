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

    val pairs = hailstones.combinations(2).toSeq

    // Sets of hailstones that have the same velocity
    val matchingVx = for 
        h <- hailstones.toSet
        matchVx = (for h2 <- hailstones if h != h2 && h.vx == h2.vx yield h2) if matchVx.length > 0
    yield matchVx.toSet + h

    val matchingVy = for 
        h <- hailstones.toSet
        matchVy = (for h2 <- hailstones if h != h2 && h.vy == h2.vy yield h2) if matchVy.length > 0
    yield matchVy.toSet + h

    val matchingVz = for 
        h <- hailstones.toSet
        matchVz = (for h2 <- hailstones if h != h2 && h.vz == h2.vz yield h2) if matchVz.length > 0
    yield matchVz.toSet + h


    // We're after integer solutions
    // t = (x1 - x2) / dv

    def candidateVx(pair:(Hailstone, Hailstone)):Seq[Long] = 
        val (h1, h2) = pair
        val dx = Math.abs(h1.x - h2.x)
        for 
            f <- compositeFactors(dx) 
            v <- Seq(h1.vx + f, h1.vx - f, f - h1.vx, -f - h1.vx)
        yield v

    def candidateVy(pair:(Hailstone, Hailstone)):Seq[Long] = 
        val (h1, h2) = pair
        val dx = Math.abs(h1.y - h2.y)
        for 
            f <- compositeFactors(dx) 
            v <- Seq(h1.vy + f, h1.vy - f, f - h1.vy, -f - h1.vy)
        yield v

    def candidateVz(pair:(Hailstone, Hailstone)):Seq[Long] = 
        val (h1, h2) = pair
        val dz = Math.abs(h1.z - h2.z)
        for 
            f <- compositeFactors(dz) 
            v <- Seq(h1.vz + f, h1.vz - f, f - h1.vz, -f - h1.vz)
        yield v

    val vxs = (for 
        set <- matchingVx.toSeq
        pairs = set.toSeq.combinations(2)
        reduced = (for Seq(a, b) <- pairs yield candidateVx(a, b)).reduce(_.intersect(_))
    yield reduced).reduce(_.intersect(_))

    val vys = (for 
        set <- matchingVy.toSeq
        pairs = set.toSeq.combinations(2)
        reduced = (for Seq(a, b) <- pairs yield candidateVy(a, b)).reduce(_.intersect(_))
    yield reduced).reduce(_.intersect(_))

    val vzs = (for 
        set <- matchingVz.toSeq
        pairs = set.toSeq.combinations(2)
        reduced = (for Seq(a, b) <- pairs yield candidateVz(a, b)).reduce(_.intersect(_))
    yield reduced).reduce(_.intersect(_))

    println("Possible vx " + vxs)
    println("Possible vy " + vys)
    println("Possible vz " + vzs)

    // Collisions only happen forwards in time. We must either be "behind" and going faster, or "ahead" and going slower

    // These sort both the sign of the velocity and the range that the position can start within

    val vx = (for 
        vx <- vxs
        slowerThan = hailstones.filter(_.vx > vx)
        fasterThan = hailstones.filter(_.vx < vx)

        // We must therefore be "ahead" of the largest x in slowerThan
        aheadOf = slowerThan.map(_.x).max
        behind = fasterThan.map(_.x).min if aheadOf < behind
    yield
        println(s"VX: $vx $aheadOf $behind")
        vx)(0)

    val vy = (for 
        vy <- vys
        slowerThan = hailstones.filter(_.vy > vy)
        fasterThan = hailstones.filter(_.vy < vy)

        // We must therefore be "ahead" of the largest x in slowerThan
        aheadOf = slowerThan.map(_.y).max
        behind = fasterThan.map(_.y).min if aheadOf < behind
    yield
        println(s"VY: $vy $aheadOf $behind")
        vy)(0)

    val vz = (for 
        vz <- vzs
        slowerThan = hailstones.filter(_.vz > vz)
        fasterThan = hailstones.filter(_.vz < vz)

        // We must therefore be "ahead" of the largest x in slowerThan
        aheadOf = slowerThan.map(_.z).max
        behind = fasterThan.map(_.z).min if aheadOf < behind
    yield
        println(s"VZ: $vz $aheadOf $behind")
        vz)(0)


    // For my input, this gives us
    // VX: 242 140479173011833 141278788412130
    // VY: 83 224188413045844 224547131890092
    // VZ: 168 205903455579299 209088305076919


    // a = (242 - h.vx) . t + h.x
    // b = (83 - h.vy) . t + h.y


    // dv = dx / t
    //


    
    // r = x + (vx - vr) t



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

