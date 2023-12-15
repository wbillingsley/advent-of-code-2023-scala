// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star29/solver.scala
// (or select the "star29" branch from GitHub)

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


def hashScore(s:String) = 
    s.foldLeft(0) { case (t, c) => 
        (t + c) * 17 % 256
    }

case class Label(l:String, n:Int)

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val puz = lines(0)
    val strings = puz.split(',')
    // println("Result is " + strings.map(hashScore).sum)

    var arrangement:Map[Int, Queue[Label]] = (for i <- 0 until 256 yield i -> Queue.empty).toMap

    strings.foreach {
        case s"$l-" => 
            val box = hashScore(l)
            arrangement = arrangement.updated(box, arrangement(box).filter(_.l != l))
            ()
        case s"$l=$n" =>
            val focal = n.toInt
            val box = hashScore(l)
            val old = arrangement(box).indexWhere(_.l == l)
            if old >= 0 then 
                arrangement = arrangement.updated(box, arrangement(box).updated(old, Label(l, focal)))
            else 
                arrangement = arrangement.updated(box, arrangement(box).enqueue(Label(l, focal)))

    }

    val overallScore = (for 
        (boxNum, q) <- arrangement
        (lens, order) <- q.zipWithIndex
    yield (boxNum + 1) * (order + 1) * lens.n).sum

    println("overall " + overallScore)



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

