import scala.io.*

// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star5/solver.scala
// (or select the "star5" branch from GitHub)


type Coord = (Int, Int)

// Ok, I guessed we'd have vertical numbers and I was wrong!
type CoordRange = (Coord, Coord)

type PartNumber = (Int, CoordRange)

// Whether a coordinate is adjacent (including diagonally) from a range
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

// Just the asterisks. We only do this twice, so let's not shrink the code duplication yet
def asteriskCoordinates(lines:Seq[String]):Seq[Coord] = 
    for 
        (line, y) <- lines.zipWithIndex
        (ch, x) <- line.zipWithIndex if ch == '*' 
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
    val astCoords = asteriskCoordinates(lines)
    
    val partNumbers = for 
        (line, y) <- lines.zipWithIndex 
        (pn, range) <- numbersInLine(y, line.trim()) if symbolCoords.exists((c) => isAdjacent(c, range))
    yield (pn, range)

    // a gear is an asterisk adjacent to two partNumbers
    val gears = 
        for asterisk <- astCoords yield
            val pns = for 
                (pn, range) <- partNumbers if isAdjacent(asterisk, range)
            yield (pn, range)

            if pns.length == 2 then 
                val (a, _) = pns(0)
                val (b, _) = pns(1)
                a * b
            else 0

    println(gears)
    println(s"Sum of gears is ${gears.sum}")

