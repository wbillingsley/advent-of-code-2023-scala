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

// Turns out all the nouns are fixed, in this order.
val nouns = Seq("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location")

// We can therefore run through the conversions we need by zipping with the tail
val mappingSuccession = nouns.zip(nouns.tail)

// We'll use this to read the data in. Then we'll work with it.
import scala.collection.mutable

case class Mapping(sourceStart:Long, destStart:Long, length:Long) {
    def sourceIncludes(n:Long):Boolean = 
        n >= sourceStart && n < sourceStart + length

    def destIncludes(n:Long):Boolean = 
        n >= destStart && n < destStart + length

    def dest(source:Long):Long = 
        source - sourceStart + destStart

    def get(n:Long):Option[Long] = 
        if sourceIncludes(n) then Some(dest(n)) else None
}

class Mapper(val from:String, val to:String) {
    private val mappings = mutable.Buffer.empty[Mapping]

    def addMapping(m:Mapping):Unit = 
        mappings.append(m)

    def sourceContains(n:Long):Boolean =
        mappings.exists((m) => m.sourceIncludes(n))

    def get(n:Long):Long = 
        mappings.find(_.sourceIncludes(n)).map(_.dest(n)).getOrElse(n)


}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    // let's read this one in mutably.

    var seeds = Seq.empty[Long]
    var maps = mutable.Map.empty[(String, String), Mapper]
    var currentMapper:Option[Mapper] = None

    var accumulateNouns = Set.empty[String]
    def nouns = accumulateNouns.toSeq

    lines.foreach {
        case seedsRegex(numbers) => 
            seeds = numbers.trim.split(' ').map(_.toLong)
            println(s"Seeds are $seeds")
        case mapRegex(from, to) => 
            println(s"Found map from $from to $to")
            accumulateNouns = accumulateNouns ++ Set(from, to)
            val m = Mapper(from, to)
            maps((from, to)) = m
            currentMapper = Some(m)
        case s if s.isBlank() => 
            ()
        case s => 
            val Array(destStart, sourceStart, length) = s.split(' ').map(_.trim).map(_.toLong)
            for m <- currentMapper do
                val mapping = Mapping(sourceStart, destStart, length)
                m.addMapping(mapping)

    } 

    val locs = for s <- seeds yield
        mappingSuccession.foldLeft(s) { case (number, (from, to)) =>
            val m = maps((from, to))
            m.get(number)
        }

    println(s"Lowest was ${locs.min}")




    

