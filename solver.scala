import scala.io.*

def value(s:String):Int = 
    println(s)
    val digits = s.filter(_.isDigit)
    println(digits)
    val d = digits.head.asDigit * 10 + digits.last.asDigit
    println(d)
    d

@main def main() = 
    val file = Source.fromFile("input.txt").getLines()
    println(s"Sum is ${file.map(value).sum}")
