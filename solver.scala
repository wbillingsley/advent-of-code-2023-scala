import scala.io.*



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
    println(found.toSeq)
    found.toSeq

case class Card(num:Int, winning:Seq[Int], have:Seq[Int]) {

    def matching = have.count(winning.contains)

    def score:Int = if matching > 0 then 
        Math.pow(2, matching - 1).toInt
    else 0

}

def lineToCard(s:String):Card = {
    println("s is")
    println(s)
    s match {
        case card(id, text) => 
            println("id is " + id)
            println("text is " + text)
            val Seq(winningText, haveText) = text.split('|').toSeq
            println("winning is " + winningText)
            val winning = numbersInText(winningText)
            val have = numbersInText(haveText)
            Card(id.toInt, winning, have)

        case x => 
            println(s"Unmatched $x")
            throw new IllegalArgumentException(".")
    }
}


@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val cards = lines.map(lineToCard)
    val scores = cards.map(_.score)
    println(scores)
    println(s"Sum is ${scores.sum}")
    

