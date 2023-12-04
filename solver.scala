import scala.io.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec

// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star7/solver.scala
// (or select the "star7" branch from GitHub)

// A regex to capture numbers in lines
val number = raw"(\d+)".r
val card = raw"Card\s+(\d+): (.*)".r

// I still had this left from yesterday, so let's just modify it
def numbersInText(line:String):Seq[Int] = 
    number.findAllIn(line).map(_.toInt).toSeq

case class Card(num:Int, winning:Seq[Int], have:Seq[Int])(cards: => Seq[Card]) {

    def matching = have.count(winning.contains)

    //No such thing as score any more
    //lazy val score:Int = if matching > 0 then 
    //    Math.pow(2, matching - 1).toInt
    //else 0

    // Let's keep track of which cards we win from this card
    lazy val wins:Seq[Int] = Range(num + 1, num + 1 + matching).toSeq

    // This is a little like Fibonacci -- the value of the next one in the sequence depends on the previous one.
    // We're going to need some memoization, but we can use a lazy val in each card instead of fold
    lazy val countOfThis:Int =
        1 + (for n <- (1 until num) if cards(n).wins.contains(num) yield cards(n).countOfThis).sum

}

def lineToCard(s:String)(getCards: => Seq[Card]):Card = {
    s match {
        case card(id, text) => 
            val Seq(winningText, haveText) = text.split('|').toSeq
            val winning = numbersInText(winningText)
            val have = numbersInText(haveText)
            Card(id.toInt, winning, have)(getCards)

        case x => 
            println(s"Unmatched $x")
            throw new IllegalArgumentException(".")
    }
}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    lazy val cards:Seq[Card] = Card(0, Nil, Nil)(cards) +: lines.map(lineToCard(_)(cards))
    // for c <- cards if c.num >= 1 do
    //     println(s"Card ${c.num} occurs ${c.countOfThis} times and wins ${c.wins}")
    

    //val won = cardCollector(cardMap, Nil, Queue(cards*))
    println(s"Won ${cards.tail.map(_.countOfThis).sum} cards")
    

