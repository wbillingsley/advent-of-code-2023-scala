// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star43/solver.scala
// (or select the "star43" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable

case class Brick(x:Range, y:Range, z:Range) {

    // Drop a brick onto a compact stack
    def fallOnto(pile:Seq[Brick]) = 
        val zValues = pile.filter((support) => x.intersect(support.x).nonEmpty && y.intersect(support.y).nonEmpty).map(_.z.end)
        val lowestFreeZ = if zValues.isEmpty then 1 else zValues.max + 1
        Brick(x, y, lowestFreeZ to lowestFreeZ + (z.end - z.start))

    // For sanity-checking that canFall didn't muck things up
    def volume = x.length * y.length * z.length

    // whether the x and y ranges of two bricks overlap
    def xyIntersect(b:Brick):Boolean = {
        x.clips(b.x) && y.clips(b.y)
    }
}

def decompose(line:String):Brick = 
    val s"$a,$b,$c~$x,$y,$z" = line
    val (aa, bb, cc) -> (xx, yy, zz) = ((a.toInt, b.toInt, c.toInt) -> (x.toInt, y.toInt, z.toInt))
    Brick(aa to xx, bb to yy, cc to zz)

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val bricks = lines.map(decompose)

    // A check that the "clip" function was working
    println((0 to 1).clips(1 to 1))

    // Check no bricks are listed upside-down
    //println(bricks.forall { (b) => (b.z.start <= b.z.end) })

    val sortedByBase = bricks.sortBy(_.z.start)

    // Returns the list of bricks that can fall
    def canFall(pile:Seq[Brick]) = pile.filter { (b) => 
        b.z.start > 1 && !pile.exists((b2) => b2.z.end == b.z.start - 1 && b.xyIntersect(b2))
    }

    // Drops a pile of bricks
    def fall(oldPile:Seq[Brick]):Seq[Brick] = 
        oldPile.foldLeft[Seq[Brick]](Nil) { (pile, b) => pile :+ b.fallOnto(pile) }

 
    val fallen = fall(sortedByBase)

    // It turned out we didn't need any memoisation.
    // This just tail-recursively eliminates bricks that can fall,
    // and the fact that the pile gets smaller every time seems to be enough to make it fast enough.
    @tailrec
    def chainReact(pile:Seq[Brick], count:Long = 0):Long = {
        val step = canFall(pile)
        if step.length == 0 then count else 
            chainReact(pile.diff(step), count + step.size)
    }

    val answers = for b <- fallen yield chainReact(fallen.filter(_ != b))

    println(answers.sum)


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

