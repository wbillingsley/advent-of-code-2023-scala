import scala.io.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec



// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star5/solver.scala
// (or select the "star5" branch from GitHub)

// A regex to capture numbers in lines
val number = raw"(\d+)".r
val card = raw"Card\s+(\d+): (.*)".r

// I still had this left from yesterday, so let's just modify it
def numbersInText(line:String):Seq[Int] = 
    val found = scala.collection.mutable.Buffer.empty[Int]
    val mi = number.findAllIn(line)
    if !mi.isEmpty then
        while mi.hasNext do
            val num = mi.next()
            found.append(num.toInt)
    found.toSeq

case class Card(num:Int, winning:Seq[Int], have:Seq[Int]) {

    def matching = have.count(winning.contains)

    //lazy val score:Int = if matching > 0 then 
    //    Math.pow(2, matching - 1).toInt
    //else 0

    // Let's keep track of which cards we win from this card
    lazy val wins:Seq[Int] = Range(num + 1, num + 1 + matching).toSeq

}

def lineToCard(s:String):Card = {
    s match {
        case card(id, text) => 
            val Seq(winningText, haveText) = text.split('|').toSeq
            val winning = numbersInText(winningText)
            val have = numbersInText(haveText)
            Card(id.toInt, winning, have)

        case x => 
            println(s"Unmatched $x")
            throw new IllegalArgumentException(".")
    }
}

@tailrec
def cardCollector(cardMap:Map[Int, Card], done:List[Card], toProcess:Queue[Card]):List[Card] = {
    if toProcess.isEmpty then done
    else
        val (card, remaining) = toProcess.dequeue
        //println(s"Processing ${card.num} which wins ${card.wins}; done:${done.map(_.num)}; toProcess:${remaining.map(_.num)}")
        val won = for 
            n <- card.wins if cardMap.contains(n)
        yield cardMap(n)
        cardCollector(cardMap, card :: done, remaining.enqueueAll(won))
}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val cards = lines.map(lineToCard)
    val cardMap = cards.map((c) => c.num -> c).toMap
    val won = cardCollector(cardMap, Nil, Queue(cards*))
    println(s"Won ${won.length} cards")
    

