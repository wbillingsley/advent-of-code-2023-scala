import scala.io.*

// This is the solution to part 2.
// For the part 1 solution, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star1/solver.scala

// We'll need this when searching for number words
val numbers = (for 
    (text, i) <- Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex
yield text -> (i + 1)).toMap

// The number of letters in the longest number
val longestNum = numbers.keySet.map(_.length).max

// Padding for a string to ensure a sliding window won't stop early
val padding = " " * longestNum

// Reads a digit (in either numeric or word form) from the start of a string if possible
def digitify(s:String):Option[Int] = 
    if s.isEmpty then None
    else if s(0).isDigit then Some(s(0).asDigit)
    else for 
        n <- numbers.keySet.find(s.startsWith(_))
    yield numbers(n)

// Uses a "sliding window" over the string to turn a String into a sequence of the digits found at that point
// (and then composes the first and last into a two-digit number)
def value(s:String):Int = 
    println(s)
    val digits = (for 
        window <- (s + padding).sliding(longestNum) 
        d <- digitify(window)
    yield d).toSeq
    println(digits)
    val d = digits.head * 10 + digits.last
    println(d)
    d

@main def main() = 
    val file = Source.fromFile("input.txt").getLines()
    println(s"Sum is ${file.map(value).sum}")
