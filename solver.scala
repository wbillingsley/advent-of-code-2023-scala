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

def ctw(s:String, hashgrps:List[Int]):Long = {
    val cache = mutable.Map.empty[(String, List[Int]), Long]

    def countTheWays(currentGroup:Option[Int], remaining:String, remainingGroups:List[Int]):Long = {
        if remaining.isEmpty() then 
            if remainingGroups.isEmpty && (currentGroup.isEmpty || currentGroup.contains(0)) then 1L else 0
        else if remaining.count((c) => c == '#' || c == '?') < remainingGroups.sum then 0
        else if remaining.length < remainingGroups.sum + remainingGroups.length - 1 then 0 // Not enough space left for the groups
        else
            val ch = remaining.head
            (ch, currentGroup) match {
                case ('.', Some(r)) if r > 0 => 0 // We have met a dot before we have completed this group of hashes
                case ('.', _) => countTheWays(None, remaining.tail, remainingGroups) // A dot, but we're outside of a group
                case ('#', Some(r)) if r <= 0 => 0 // This group of hashes is too long
                case ('#', Some(r)) => countTheWays(Some(r - 1), remaining.tail, remainingGroups)
                case ('#', None) => remainingGroups match {
                    case h :: t => countTheWays(Some(h - 1), remaining.tail, t)
                    case _ => 0 // out of hashes
                }
                case ('?', Some(r)) if r > 0 => countTheWays(Some(r - 1), remaining.tail, remainingGroups) // assume #
                case ('?', Some(r)) => countTheWays(None, remaining.tail, remainingGroups) // assume . -- we need a . to separate the group
                case ('?', None) => remainingGroups match {
                    case h :: t => 
                        countTheWays(Some(h - 1), remaining.tail, t)  // if #
                        + cache.getOrElseUpdate((remaining.tail, remainingGroups), countTheWays(None, remaining.tail, remainingGroups)) 

                    case _ => 
                        countTheWays(None, remaining.tail, remainingGroups) // Must be .
                }
            }

    }

    countTheWays(None, s, hashgrps)

}

case class ConditionReport(individual:String, byGroup:String) {

    val springCount = individual.length

    val brokenCounts = integers.findAllIn(byGroup).map(_.toInt).toSeq

    val brokenFLE = brokenCounts.map((x) => false -> x)

    val totalBroken = brokenCounts.sum

    def gaps = brokenCounts.length - 1

    def spaceToDistribute = springCount - totalBroken

    lazy val possibilities = 
        ctw(individual, brokenCounts.toList)

    val unknowns = individual.filter(_ == '?').length

    def pp() = 
        println(this)
        println(s"Distribute $spaceToDistribute across $gaps gaps")
        // println(s"length ${individual.length()} unkowns $unknowns broken $brokenCounts totalling $totalBroken with ${brokenCounts.length - 1} spaces")
        println()

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
    val lines = Source.fromFile("input.txt").getLines().toSeq

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




    














    

