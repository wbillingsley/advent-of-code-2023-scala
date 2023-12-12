// This is the solution for part 2
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star19/solver.scala
// (or select the "star20" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*

type Line = Seq[Int]

val integers = "(-?\\d+)".r
val decimals = "(-?\\d+)([.]\\d+)?".r

def allIntegersIn(s:String) = integers.findAllIn(s).map(_.toLong).toSeq
def allDecimalsIn(s:String) = decimals.findAllIn(s).map(_.toDouble).toSeq

// true == operational
// false == damaged

// Ye olde run-length-encoding
def rle[T](s:Seq[T]):List[(T, Int)] = 
    @tailrec
    def _rle(s:Seq[T], soFar:List[(T, Int)]):List[(T, Int)] =
        if s.isEmpty then soFar else
            val h = s.head
            val seq = s.takeWhile(_ == h)
            _rle(s.drop(seq.length), (h -> seq.length) :: soFar)

    _rle(s, Nil).reverse

case class LazyChain[T](heads:Seq[T])(_tail: () => Seq[LazyChain[T]]) {

    lazy val tail = _tail()

    def forall(f: T => Boolean):Boolean =
        heads.forall(f) && tail.forall(_.forall(f))

    // Compare this with a string representation, using a test function, bailing lazily if it fails
    def stringMatch(input:String)(f: T => String)(compare: (String, String) => Boolean):Int = 
        val headStrings = heads.map(f).mkString
        val matchTo = input.take(headStrings.length())

        if compare(headStrings, matchTo) then 
            val nextInput = input.drop(headStrings.length)
            if tail.isEmpty then 1 else 
                tail.map(_.stringMatch(nextInput)(f)(compare)).sum
        else 0
}


def lazyDistribute[T](value:T, number:Int, into:Seq[(T, Int)])(first:Boolean = false):Seq[LazyChain[(T, Int)]] = 
    if number == 0 then Seq.empty else 
        val keep = into.length - 1 // only the first and last buckets can have zero items
        val start = if first then 0 else 1

        for 
            n <- start to number - keep
        yield
            if into.isEmpty then 
                LazyChain(Seq(value -> n))(() => Seq.empty)
            else 
                LazyChain(Seq(value -> n, into.head))(() => lazyDistribute(value, number - n, into.tail)(false))
        




def brokenGroups(s:Seq[Boolean]) = 
    rle(s).filter({ (c, n) => !c }).map({ (c, n) => n })

case class ConditionReport(individual:String, byGroup:String) {

    val springCount = individual.length

    val brokenCounts = integers.findAllIn(byGroup).map(_.toInt).toSeq

    val brokenFLE = brokenCounts.map((x) => false -> x)

    val totalBroken = brokenCounts.sum

    def gaps = brokenCounts.length - 1

    def spaceToDistribute = springCount - totalBroken

    lazy val possibilities = 
        val all = lazyDistribute(true, spaceToDistribute, brokenFLE)(true)
        all.map(_.stringMatch(individual)({ (v, n) => 
            if v then "." * n else "#" * n 
        })({ (a, b) =>
            a.zip(b).forall { (aa, bb) => 
                aa == bb || aa == '?' || bb == '?'
            }
        })).sum

    val unknowns = individual.filter(_ == '?').length

    def compatibleByCharacter(c:Char, b:Boolean):Boolean = c match {
        case '?' => true
        case '.' => b
        case '#' => !b
    }

    def compatibleA(b:Seq[Boolean]):Boolean = 
        individual.zip(b).forall { (c, bb) => compatibleByCharacter(c, bb) }

    def compatibleG(b:Seq[Boolean]):Boolean = 
        brokenGroups(b).mkString(",") == byGroup   


    def merge(s:List[Boolean]):Seq[Boolean] = 
        var cursor = s
        individual.map { 
            case '?' => 
                val h :: t = cursor : @unchecked
                cursor = t
                h
            case '.' => true
            case '#' => false
        }

    def pp() = 
        println(this)
        println(s"Distribute $spaceToDistribute across $gaps gaps")
        // println(s"length ${individual.length()} unkowns $unknowns broken $brokenCounts totalling $totalBroken with ${brokenCounts.length - 1} spaces")
        println()

    def compatible = 
        println(s"Calculating compatible for ${this} with ${unknowns} unknowns")
        for 
            i <- 0 until Math.pow(2, unknowns).toInt 
            binary = merge(toBinary(i, unknowns)) if compatibleG(binary)
        yield binary

}

// take an int, and convert it to a Seq of true/false
def toBinary(i:Int, l:Int):List[Boolean] = 
    val ltr = Seq.tabulate(l) { (x) => 
        (i & (1 << x)) != 0 
    }
    ltr.reverse.toList

def decompose(line:String):ConditionReport =
    val Array(indiv, byGroup) = line.split(' ')
    val unfoldedI = Seq.fill(5)(indiv).mkString("?")
    val unfoldedG = Seq.fill(5)(byGroup).mkString(",")
    ConditionReport(unfoldedI, unfoldedG)

@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq

    val puzzle = lines.map(decompose)
    println(puzzle.head)

    val numUnk = puzzle.map(_.unknowns).max
    println(s"Max is $numUnk unknowns")


    val pos = for c <- puzzle yield 
        c.pp()
        println(c.possibilities)
        println("---")
        c.possibilities

    println(s"Sum is ${pos.sum}")

    // val solutions = puzzle.map(_.compatible.size)
    // println(solutions.sum)


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

