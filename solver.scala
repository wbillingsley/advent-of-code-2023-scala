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

case class Brick(x:Range, y:Range, z:Range)

def decompose(line:String):Brick = 
    val s"$a,$b,$c~$x,$y,$z" = line
    val (aa, bb, cc) -> (xx, yy, zz) = ((a.toInt, b.toInt, c.toInt) -> (x.toInt, y.toInt, z.toInt))
    Brick(aa to xx, bb to yy, cc to zz)

@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq

    val bricks = lines.map(decompose)

    // Check no bricks are listed upside-down
    //println(bricks.forall { (b) => (b.z.start <= b.z.end) })

    val sortedByBase = bricks.sortBy(_.z.start)
    
    def canFall = bricks.count { (b) => 
        b.z.start > 1 && !bricks.exists((support) => b.x.intersect(support.x).nonEmpty && b.y.intersect(support.y).nonEmpty && support.z.contains(b.z.end + 1))
    }

    println(s"Can fall: $canFall")


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

