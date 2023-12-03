import scala.io.*

// This is the solution for part 1
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star3/solver.scala
// (or select the "star3" branch from GitHub)


type Coord = (Int, Int)
type CoordRange = (Coord, Coord)
type PartNumber = (Int, CoordRange)

def isAdjacent(coord:Coord, range:CoordRange):Boolean = 
    val ((minX, minY), (maxX, maxY)) = range
    val (x, y) = coord
    x >= minX - 1 && x <= maxX && y >= minY - 1 && y <= maxY

// A regex to capture numbers in lines
val number = raw"(\d+)".r

// Assume symbols are not letters, digits, or dots or spaces
def symbolCoordinates(lines:Seq[String]):Seq[Coord] = 
    for 
        (line, y) <- lines.zipWithIndex
        (ch, x) <- line.zipWithIndex if !ch.isDigit && !ch.isLetter && (!". ".contains(ch)) 
    yield (x, y)   

// I'm not a big fan of MatchIterator not being an Iterator of matches (it's an iterator of Strings) but oh well
def numbersInLine(y:Int, line:String):Seq[PartNumber] = 
    val found = scala.collection.mutable.Buffer.empty[PartNumber]
    val mi = number.findAllIn(line)
    if !mi.isEmpty then
        while mi.hasNext do
            val num = mi.next()
            found.append((num.toInt, ((mi.start, y), (mi.end, y + 1))))
    println(found.toSeq)
    found.toSeq
            
        

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val symbolCoords = symbolCoordinates(lines)
    println(symbolCoords)

    println("""

    ---
    
    """)
    val partNumbers = for 
        (line, y) <- lines.zipWithIndex 
        (pn, range) <- numbersInLine(y, line.trim()) if symbolCoords.exists((c) => isAdjacent(c, range))
    yield pn
    println(partNumbers)
    println(partNumbers.length)
    val sum = partNumbers.sum
    println(s"Sum of part numbers is ${sum}")

