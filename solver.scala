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

type LongRange = scala.collection.immutable.NumericRange[Long]

extension (r1:LongRange) {
    def clip(r2:LongRange):(LongRange, LongRange, LongRange) = {
        val overlapStart = Math.max(r1.start, r2.start)
        val overlapEnd = Math.min(r1.end, r2.end)

        (
            Range.Long(r1.start, overlapStart, 1), // untransformed "before" fragment
            Range.Long(overlapStart, overlapEnd, 1), // overlap
            Range.Long(overlapEnd, r1.end, 1) // untransformed "after" fragment
        )
    }

    def clips(r2:LongRange) = 
        Math.max(r1.start, r2.start) < Math.min(r1.end, r2.end)

    def shifted(delta:Long) = 
        Range.Long(r1.start + delta, r1.end + delta, 1)

}

// The Scala API's ranges only take lengths as Ints. Oh well, let's write our own
// class LongRange(s:Long, e:Long) {

//     def start = s

//     def isEmpty:Boolean = e <= start

//     def contains(l:Long):Boolean = l >= start && l < exclEnd

//     def intersects(r:LongRange):Boolean = 
//         Math.max(start, r.start) < Math.min(exclEnd, r.exclEnd)

//     def length = if isEmpty then 0L else e - start

//     def exclEnd = if isEmpty then start else e

//     def shifted(i:Long) = LongRange(s + i, e + i)

//     def take(length:Long) = LongRange(s, Math.min(exclEnd, s + length))

//     def drop(length:Long) = LongRange(Math.min(exclEnd, s + length), exclEnd)

//     def takeRight(length:Long) = LongRange(Math.max(s, exclEnd - length), exclEnd)

//     def intersect(r:LongRange):(LongRange, LongRange, LongRange) = 
//         val overlapStart = Math.max(start, r.start)
//         val overlapEnd = Math.min(exclEnd, r.exclEnd)

//         (
//             LongRange(start, overlapStart), // untransformed "before" fragment
//             LongRange(overlapStart, overlapEnd), // transformed middle
//             LongRange(overlapEnd, exclEnd) // untransformed "after" fragment
//         )

//     override def toString = s"[$start, $exclEnd)"

// }


case class Mapping(from:LongRange, delta:Long) {
    def contains(l:Long) = from.contains(l)

    def to = from.shifted(delta)

    def take(length:Long) = Mapping(from.take(length.toInt), delta)

    def takeRight(length:Long) = 
        val drop = Math.max(from.length - length, 0)
        Mapping(from.drop(drop.toInt), delta)

    def drop(length:Long) = Mapping(from.drop(length.toInt), delta)
    
    def shifted(d:Long) = Mapping(from, delta + d)
     
    def apply(n:Long) = 
        if contains(n) then n + delta else throw IllegalArgumentException(s"$from until $to doesn't contain $n")

    def transform(r:LongRange):(LongRange, LongRange, LongRange) = {
        val (before, overlap, after) = r.clip(from)

        (before, overlap.shifted(delta), after)
    }

    // Applies this mapping to another mapping
    def transformMapping(m:Mapping):(Mapping, Mapping, Mapping) = {
        val (before, overlap, after) = transform(m.to)
        (
            m.take(before.length),
            m.drop(before.length).take(overlap.length).shifted(delta),
            m.takeRight(after.length)
        )
    }

}


class Mapper(val from:String, val to:String) {
    private val _mappings = mutable.Buffer.empty[Mapping]

    def addMapping(m:Mapping):Unit = 
        if !m.from.isEmpty then 
            _mappings.append(m) 

    def mappings = _mappings.toSeq.sortBy(_.from.start)

    def apply(n:Long):Option[Long] = 
        mappings.find(_.contains(n)).map(_.apply(n))

    def fillBlanks():Unit = 
        println(s"Filling in the blanks for $from to $to")

        var last = 0L
        val fill = (for 
            i <- mappings
            m <- {
                println(s"found mapping $i; last is $last")
                if i.from.start == last then 
                    last = i.from.end
                    Seq.empty
                else
                    println(s"filling a blank from $last to ${i.from.start}") 
                    last = i.from.end
                    Seq(Mapping(Range.Long(last, i.from.start, 1), 0))
            }
        yield m) :+ Mapping(Range.Long(last, Long.MaxValue, 1), 0)

        for m <- fill do addMapping(m)

        println(this)
        println

    
    def transform(m:Mapper):Mapper = {
        val result = Mapper(m.from, to)

        for section <- m.mappings do
            val intersections = mappings.filter(_.from.clips(section.to)).sortBy(_.from.start)
            val last = intersections.foldLeft(section) { case (remaining, transform) => 
                val (b, o, a) = transform.transformMapping(remaining)
                result.addMapping(b)
                result.addMapping(o)
                a
            }
            result.addMapping(last)

        result


    }

    override def toString():String = s"""|Mapper from $from to $to
                       |""".stripMargin +
                       (for (m <- mappings) yield m.toString + "\n")

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
            // We've finished the current mapper, so fill in any gaps
            for m <- currentMapper do m.fillBlanks()
        
        case s => 
            val Array(destStart, sourceStart, length) = s.split(' ').map(_.trim).map(_.toLong)
            for m <- currentMapper do
                val mapping = Mapping(Range.Long(sourceStart, sourceStart + length, 1), destStart - sourceStart)
                m.addMapping(mapping)

    } 

    val identityMapper = Mapper("seed", "seed")
    for Seq(start, range) <- seeds.grouped(2) do
        identityMapper.addMapping(Mapping(Range.Long(start, start + range, 1), 0))

    println(identityMapper)

    println(maps(("temperature", "humidity")))

    val composed = mappingSuccession.foldLeft(identityMapper) { case (mapper, (from, to)) =>
        val m = maps((from, to))
        val composed = m.transform(mapper)

        println(composed)
        println(composed.apply(82))

        composed
    }


    println(composed.mappings.map(_.to).filter(!_.isEmpty).map(_.start).min)



    

