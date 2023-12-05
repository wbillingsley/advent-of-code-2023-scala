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

    def get(n:Long):Option[Long] = 
        if sourceIncludes(n) then Some(dest(n)) else None
}

class Mapper(val from:String, val to:String) {
    private val mappings = mutable.Buffer.empty[Mapping]

    def addMapping(m:Mapping):Unit = 
        mappings.append(m)

    def sourceContains(n:Long):Boolean =
        mappings.exists((m) => m.sourceIncludes(n))

    def get(n:Long):Option[Long] = 
        mappings.find(_.sourceIncludes(n)).map(_.dest(n))


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

    // Let's do this the sneaky mutable way.
    // ensure there is a mapping from everything to everything
    for 
        from <- nouns
        to <- nouns if to != from && !maps.contains((from, to))
    do
        maps((from, to)) = Mapper(from, to)


    // Given a mapping, updates everything else it can possibly know
    def transitivelyUpdateMapping(m:Mapper, source:Long, dest:Long):Unit = {
        //if !m.sourceContains(source) then
            m.addMapping(Mapping(source, dest, 1))

            println()

            for 
                transitiveMap <- maps.values.filter(_.from == m.to)
                inferredDest <- transitiveMap.get(dest)
            do
                println(s"Also transitively mapping from ${m.from} $source to ${transitiveMap.to} $inferredDest")
                transitivelyUpdateMapping(maps((m.from, transitiveMap.to)), source, inferredDest)
    }

    val seedMaps:Map[String, Mapper] = (for n <- nouns.toSeq if n != "seed" yield
        val map = maps.getOrElse(("seed", n), Mapper("seed", n))
        n -> map).toMap

    val locationMap = seedMaps("location")


    for s <- seeds do 
        for 
            m <- seedMaps.values 
            d <- m.get(s)
        do 
            println(s"Found seed map from $s to ${m.to} $d")
            // hacky 
            transitivelyUpdateMapping(m, s, d)

    //for s <- seeds do
    println("Missing:" + seeds.filter((s) => locationMap.get(s).isEmpty)) 


    

