// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star13/solver.scala
// (or select the "star13" branch from GitHub)

import scala.io.*

val cardMap = Seq(
    'A' -> 14, 'K' -> 13, 'Q' -> 12, 'T' -> 10,
    '9' -> 9, '8' -> 8, '7' -> 7, '6' -> 6, '5' -> 5, '4' -> 4, '3' -> 3, '2' -> 2, 
    'J' -> 0, 
).toMap

enum HandType:
    case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

import HandType.*

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
    val best = set.maxBy((s) => Hand(s).handValue)
    Hand(s, Some(Hand(best).typ))
}

case class Hand(s:String, typeOverride:Option[HandType] = None) {

    val grouplengths = s.groupBy(identity).map((c, s) => (c, s.length))

    lazy val typ:HandType = typeOverride.getOrElse({
        if grouplengths.values.max == 5 then FiveOfAKind else 
        if grouplengths.values.max == 4 then FourOfAKind else
        if grouplengths.values.toSeq.contains(3) && grouplengths.values.toSeq.contains(2) then FullHouse else
        if grouplengths.values.toSeq.contains(3) then ThreeOfAKind else
        if grouplengths.values.count(_ == 2) == 2 then TwoPair else
        if grouplengths.values.toSeq.contains(2) then OnePair else
        HighCard
    })

    def fakeFaceValue = s.foldLeft(0) { case (t, c) => t * 16 + cardMap(c) }

    // A filthy hack!
    def handValue = (16777216 * typ.ordinal) + fakeFaceValue

}

def decompose(line:String):(Hand, Int) = 
    val s"$hand $bid" = line
    (bestHand(hand), bid.toInt)
 

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val pairs = lines.map(decompose)
    val sorted = pairs.sortBy(_._1.handValue)
    println(sorted.zipWithIndex.map({ case ((h, b), i) => b * (i + 1) }).sum )

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())













    

