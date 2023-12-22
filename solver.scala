// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable


type Coord3 = (Int, Int, Int)

type BrickData = (Coord3, Coord3)

case class Brick(x:Range, y:Range, z:Range) {

    // Drop a brick onto a compact stack
    def fallOnto(pile:Seq[Brick]) = 
        val zValues = pile.filter((support) => x.intersect(support.x).nonEmpty && y.intersect(support.y).nonEmpty).map(_.z.end)
        val lowestFreeZ = if zValues.isEmpty then 1 else zValues.max + 1
        Brick(x, y, lowestFreeZ to lowestFreeZ + (z.end - z.start))

    def volume = x.length * y.length * z.length
}

def decompose(line:String):Brick = 
    val s"$a,$b,$c~$x,$y,$z" = line
    val (aa, bb, cc) -> (xx, yy, zz) = ((a.toInt, b.toInt, c.toInt) -> (x.toInt, y.toInt, z.toInt))
    Brick(aa to xx, bb to yy, cc to zz)

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val bricks = lines.map(decompose)

    // Check no bricks are listed upside-down
    //println(bricks.forall { (b) => (b.z.start <= b.z.end) })

    val sortedByBase = bricks.sortBy(_.z.start)

    def canFall(pile:Seq[Brick]) = pile.count { (b) => 
        b.z.start > 1 && !bricks.exists((support) => b.x.intersect(support.x).nonEmpty && b.y.intersect(support.y).nonEmpty && support.z.contains(b.z.end + 1))
    }
    // println(s"Can fall: $canFall")

    def fall(oldPile:Seq[Brick]):Seq[Brick] = 
        oldPile.foldLeft[Seq[Brick]](Nil) { (pile, b) => pile :+ b.fallOnto(pile) }

 
    val fallen = fall(sortedByBase)

    // sanity check that volume has not changed
    //println(sortedByBase.map(_.volume).sum == fallen.map(_.volume).sum)

    println(fallen)

    def canDisintigrate = fallen.count { (b) => 
        val removed = fallen.filter(_ != b)
        // println(s"${fallen.length} -> ${removed.length}")
        fall(removed) == removed
        //canFall(removed) == 0
    }

    println(s"Can disintigrate ${canDisintigrate}")
    


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

