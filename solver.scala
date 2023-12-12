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

def brokenGroups(s:Seq[Boolean]) = 
    rle(s).filter({ (c, n) => !c }).map({ (c, n) => n })

case class ConditionReport(individual:String, byGroup:String) {

    val springCount = individual.length

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
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val puzzle = lines.map(decompose)
    println(puzzle.head)

    val numUnk = puzzle.map(_.unknowns).max
    println(s"Max is $numUnk unknowns")

    // val solutions = puzzle.map(_.compatible.size)
    // println(solutions.sum)


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

