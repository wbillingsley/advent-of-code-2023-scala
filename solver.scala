// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star30/solver.scala
// (or select the "star30" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*

type Line = Seq[Int]

val integers = "(-?\\d+)".r
val decimals = "(-?\\d+)([.]\\d+)?".r

def allIntegersIn(s:String) = integers.findAllIn(s).map(_.toLong).toSeq
def allDecimalsIn(s:String) = decimals.findAllIn(s).map(_.toDouble).toSeq


def hashScore(s:String) = 
    s.foldLeft(0) { case (t, c) => 
        (t + c) * 17 % 256
    }

@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq

    val puz = lines(0)
    val strings = puz.split(',')
    println("Result is " + strings.map(hashScore).sum)


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

