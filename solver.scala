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

// We'll use this to read the data in. Then we'll work with it.
import scala.collection.mutable

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    // let's read this one in mutably

    var seeds = Seq.empty[Long]
    var maps = mutable.Map.empty[(String, String), mutable.Map[Long, Long]]
    var currentMap:Option[mutable.Map[Long, Long]] = None

    lines.foreach {
        case seedsRegex(numbers) => 
            seeds = numbers.trim.split(' ').map(_.toLong)
            println(s"Seeds are $seeds")
        case mapRegex(from, to) => 
            println(s"Found map from $from to $to")
            val m = mutable.Map.empty[Long, Long]
            maps((from, to)) = m
            currentMap = Some(m)
        case s if s.isBlank() => 
            ()
        case s => 
            val numbers = s.split(' ').map(_.trim).map(_.toLong).toSeq
            println(numbers)

    } 

    

