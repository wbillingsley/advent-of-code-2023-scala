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


// How far we travel for a given button push length
def travelTimes(time:Long) = {
    for 
        i <- Range.Long(0, time, 1)
    yield 
        i * (time - i)
}


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

    def waysToWin():Int = {
        val d = distance
        travelTimes(time).filter(_ > d).length
    }

    println(waysToWin())

    // println(distances.indices.map(waysToWin).product)












    

