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
def rle[T](s:Seq[T]):Seq[(T, Int)] = 
    @tailrec
    def _rle(s:Seq[T], soFar:List[(T, Int)]):Seq[(T, Int)] =
        if s.isEmpty then soFar else
            val h = s.head
            val seq = s.takeWhile(_ == h)
            _rle(s.drop(seq.length), (h -> seq.length) :: soFar)

    _rle(s, Nil).reverse

def brokenGroups(s:Seq[Boolean]) = 
    rle(s).filter({ (c, n) => !c }).map({ (c, n) => n })

case class ConditionReport(individual:String, byGroup:String) {

    val springCount = individual.length

    def compatibleByCharacter(c:Char, b:Boolean):Boolean = c match {
        case '?' => true
        case '.' => b
        case '#' => !b
    }

    def compatibleA(b:Seq[Boolean]):Boolean = 
        individual.zip(b).forall { (c, bb) => compatibleByCharacter(c, bb) }

    def compatibleG(b:Seq[Boolean]):Boolean = 
        brokenGroups(b).mkString(",") == byGroup   


    def compatible = for 
        i <- 0 until Math.pow(2, springCount).toInt 
        binary = toBinary(i, springCount) if compatibleA(binary) && compatibleG(binary)
    yield binary

}

// take an int, and convert it to a Seq of true/false
def toBinary(i:Int, l:Int):Seq[Boolean] = 
    val ltr = Seq.tabulate(l) { (x) => 
        (i & (1 << x)) != 0 
    }
    ltr.reverse

def decompose(line:String):ConditionReport =
    val Array(indiv, byGroup) = line.split(' ')
    ConditionReport(indiv, byGroup)

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val puzzle = lines.map(decompose)

    val solutions = puzzle.map(_.compatible.size)
    println(solutions.sum)


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

