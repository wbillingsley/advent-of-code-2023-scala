import scala.io.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec

// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star11/solver.scala
// (or select the "star11" branch from GitHub)

// A regex to capture numbers in lines
val number = raw"(\d+)".r
val card = raw"Card\s+(\d+): (.*)".r

val timesRegex = raw"Time: (.*)".r
val distancesRegex = raw"Distance: (.*)".r

// val mapRegex = raw"(\w+)-to-(\w+) map:".r

import util.* 


// A more efficient range-based solution for brute-forcing it
def wins(time:Long, d:Long) = {
    Range.Long(0, time, 1).count((i) => i * (time - i) > d )
}

/// Or there's the maths way --

// If it were continuous, how much of -i ^ 2 + t * i - d > 0
def continuousSolution(t:Long, d:Long) = Math.sqrt(Math.pow(t, 2) - 4 * d)

// But it's discrete, so we need to find out the number of integers within that part of the curve
def range(t:Long, d:Long):(Long, Long) =
    val s = continuousSolution(t, d) // amount of the curve where > d
    val c = t.toDouble / 2 // centre of curve
    Math.ceil(c - s/2).toLong -> Math.floor(c + s/2).toLong // inclusive range of integers where it beats d

// Now the wins is just a subtraction
def mathWins(t:Long, d:Long): Long = 
    val (a, b) = range(t, d)
    b - a + 1

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    // Today's file is short and in a fixed format. Let's just get it in
    val time = lines(0) match {
        case timesRegex(text) =>             
            number.findFirstIn(text.filter(!_.isWhitespace)).get.toLong
    }
    val distance = lines(1) match {
        case distancesRegex(text) => 
            number.findFirstIn(text.filter(!_.isWhitespace)).get.toLong
    }

    println(wins(time, distance))
    println(mathWins(time, distance))












    

