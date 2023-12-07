// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star13/solver.scala
// (or select the "star13" branch from GitHub)

import scala.io.*

enum HandType:
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

import HandType.*

def part1 = "23456789TJQKA"
def part2 = "J23456789TJQKA"

def typeOf(hand:String):HandType = 
    val grouplengths = hand.groupBy(identity).map((c, str) => str.length).toSeq

    if grouplengths.max == 5 then FiveOfAKind else 
        if grouplengths.max == 4 then FourOfAKind else
        if grouplengths.contains(3) && grouplengths.contains(2) then FullHouse else
        if grouplengths.contains(3) then ThreeOfAKind else
        if grouplengths.count(_ == 2) == 2 then TwoPair else
        if grouplengths.contains(2) then OnePair else
        HighCard

def convertedValue(hand:String, cardValues:String):String = 
    hand.map((c) => cardValues.indexOf(c).toChar)

def handValue(typ:HandType, s:String, cardValues:String):String = 
    typ.ordinal.toChar +: convertedValue(s, cardValues)

def rawValueOf(s:String) = 
    handValue(typeOf(s), s, part1)

def replaceJ(s:String):Seq[String] = {
    if s.contains('J') then 
        for c <- "AKQT98765432" yield
            s.replaceFirst("J", "" + c)
    else Seq(s)
}

def handMultiplier(s:Seq[String], count:Int) = {
    var result = s
    for i <- 0 until count do result = result.flatMap(replaceJ)
    result
}

def bestHand(s:String) = {
    val count = s.count(_ == 'J')
    val set = handMultiplier(Seq(s), count)
    set.maxBy[String]((s) => rawValueOf(s))
}

def wildCardValueOf(s:String) =
    val best = bestHand(s) 
    handValue(typeOf(best), s, part2)
    






def decompose(line:String):((String, String), Int) = 
    val s"$hand $bid" = line
    ((hand, wildCardValueOf(hand)), bid.toInt)


@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val pairs = lines.map(decompose)
    val sorted = pairs.sortBy({ case ((h, v), bid) => v })
    println(sorted.zipWithIndex.map({ case ((h, b), i) => b * (i + 1) }).sum )

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())













    

