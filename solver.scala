import scala.io.*

val numbers = (for 
    (text, i) <- Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex
yield text -> (i + 1)).toMap

val longestNum = numbers.keySet.map(_.length).max

// Pad the string to ensure a sliding window won't stop early
val padding = " " * longestNum

def digitify(s:String):Option[Int] = 
    if s.isEmpty then None
    else if s(0).isDigit then Some(s(0).asDigit)
    else for 
        n <- numbers.keySet.find(s.startsWith(_))
    yield numbers(n)

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
