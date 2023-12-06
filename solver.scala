import scala.io.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec

// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star9/solver.scala
// (or select the "star9" branch from GitHub)

// A regex to capture numbers in lines
val number = raw"(\d+)".r
val card = raw"Card\s+(\d+): (.*)".r

val seedsRegex = raw"seeds: (.*)".r
val mapRegex = raw"(\w+)-to-(\w+) map:".r

import util.* 

@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq
    println(Range.Long(1, 3, 1).clips(Range.Long(2, 5, 1)))




    

