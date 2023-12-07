// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star13/solver.scala
// (or select the "star13" branch from GitHub)

import scala.io.*


def part1 = "23456789TJQKA"
def part2 = "J23456789TJQKA"

import math.Ordering.Implicits.seqOrdering

def counts[T](s:Seq[T]) = 
    val c = s.groupBy(identity).values.map(_.length).toSeq
    c.sortBy(-_).reverse

def convertedValue(hand:String, cardValues:String):Seq[Int] = 
    hand.map((c) => cardValues.indexOf(c))

def rawValue(s:String) = 
    counts(s) ++ convertedValue(s, part1)

// We don't have to replace the wildcards
// Whatever the count sequence was, we should always put all wildcards on the first entry
def wildCardValue(s:String) = 
    val nj = s.count(_ == 'J')
    val h :: t = counts(s.filter(_ != 'J')).toList
    (h + nj :: t) ++ convertedValue(s, part2)


def decompose(line:String):(String, Int) = 
    val s"$hand $bid" = line
    (hand, bid.toInt)


@main def main() = 
    val lines = Source.fromFile("test.txt").getLines().toSeq
    val pairs = lines.map(decompose)

    val part1 = pairs.sortBy((s, _) => rawValue(s)).map((_, b) => b).zipWithIndex.map((a, b) => a * (b + 1)).sum
    println("Part 1 " + part1)

    val part2 = pairs.sortBy((s, _) => wildCardValue(s)).map((_, b) => b).zipWithIndex.map((a, b) => a * (b + 1)).sum
    println("Part 2 " + part2)

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())













    

