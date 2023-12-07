//> using dep org.typelevel::cats-collections-core:0.9.8


// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star13/solver.scala
// (or select the "star13" branch from GitHub)


// just in case, let's keep Diet in scope. There's been lots of range problems.
//import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._


import scala.io.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec


// A regex to capture numbers in lines
val number = raw"(\d+)".r
val card = raw"Card\s+(\d+): (.*)".r

val timesRegex = raw"Time: (.*)".r
val distancesRegex = raw"Distance: (.*)".r

// val mapRegex = raw"(\w+)-to-(\w+) map:".r

// import util.* 


@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at " + java.util.Date().toLocaleString())













    

