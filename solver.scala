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

case class Mapping(sourceStart:Long, destStart:Long, length:Long) {
    def sourceIncludes(n:Long):Boolean = 
        n >= sourceStart && n < sourceStart + length

    def destIncludes(n:Long):Boolean = 
        n >= destStart && n < destStart + length

    def dest(source:Long):Long = 
        source - sourceStart + destStart
}

class Mapper(from:String, to:String) {
    private val mappings = mutable.Buffer.empty[Mapping]

    def addMapping(m:Mapping):Unit = 
        mappings.append(m)


}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    // let's read this one in mutably

    var seeds = Seq.empty[Long]
    var maps = mutable.Map.empty[(String, String), Mapper]
    var currentMapper:Option[Mapper] = None

    lines.foreach {
        case seedsRegex(numbers) => 
            seeds = numbers.trim.split(' ').map(_.toLong)
            println(s"Seeds are $seeds")
        case mapRegex(from, to) => 
            println(s"Found map from $from to $to")
            val m = Mapper(from, to)
            maps((from, to)) = m
            currentMapper = Some(m)
        case s if s.isBlank() => 
            ()
        case s => 
            val Array(destStart, sourceStart, length) = s.split(' ').map(_.trim).map(_.toLong)
            for m <- currentMapper do
                val mapping = Mapping(sourceStart, destStart, length)
                println(mapping)
                m.addMapping(mapping)

    } 

    

