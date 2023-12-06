import scala.io.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec

// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star9/solver.scala
// (or select the "star9" branch from GitHub)

// A regex to capture numbers in lines
val number = raw"(\d+)".r
val card = raw"Card\s+(\d+): (.*)".r

val timesRegex = raw"Time: (.*)".r
val distancesRegex = raw"Distance: (.*)".r

// val mapRegex = raw"(\w+)-to-(\w+) map:".r

import util.* 

@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq

    // Today's file is short and in a fixed format. Let's just get it in
    val times = lines(0) match {
        case timesRegex(text) => 
            number.findAllIn(text).map(_.toInt).toSeq
    }
    val distances = lines(1) match {
        case distancesRegex(text) => 
            number.findAllIn(text).map(_.toInt).toSeq
    }
    println(distances)








    

