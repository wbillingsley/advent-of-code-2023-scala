// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

// Parses a line of puzzle
def decompose1(line:String) = 
    val s"$dir $dist (#$col)" = line

    val parsedDir = dir match {
        case "R" => East
        case "U" => North
        case "D" => South
        case "L" => West
    }
    (parsedDir, dist.toInt, col)



// Parses a line of puzzle
def decompose2(line:String) = 
    val s"$dir $dist (#$col)" = line

    val hexDist = col.take(5)
    val hexDistParsed = Integer.parseInt(hexDist, 16)

    val hexDir = col.last match {
        case '0' => East
        case '1' => South
        case '2' => West
        case '3' => North
        case _ => 
            println(hexDist)
            throw new IllegalArgumentException("pop")
    }

    val parsedDir = dir match {
        case "R" => East
        case "U" => North
        case "D" => South
        case "L" => West
    }
    (hexDir, hexDistParsed.toInt, "#")


// An edge in the puzzle, formed by carving a line in some direction
type Edge = (Coord, Coord) // (from, to, dir1, dir2). the last two aren't strictly necessary, but we might as well

// A region bounded by two points. We are inclusive of the top-left, and exclusive of the bottom-right
type Region = (Coord, Coord)

// Region maths
extension (r:Region) {
    def x1 = Math.min(r._1._1, r._2._1)
    def x2 = Math.max(r._1._1, r._2._1)
    def y1 = Math.min(r._1._2, r._2._2)
    def y2 = Math.max(r._1._2, r._2._2)

    def contains(p:Coord):Boolean = 
        val (x, y) = p
        r.x1 <= x && r.x2 > x && r.y1 <= y && r.y2 > y

    def corners = 
        (for x <- Seq(r.x1, r.x2); y <- Seq(r.y1, r.y2) yield (x, y))

    def intersects(r2:Region):Boolean = 
        val ((x1, y1), (x2, y2)) = r2
        r2.corners.exists((p) => r.contains(p)) || r.corners.exists((p) => r2.contains(p))

    def intersect(r2:Region):Region = 
        val x1 = Math.max(r.x1, r2.x1)
        val x2 = Math.min(r.x2, r2.x2)
        val y1 = Math.max(r.y1, r2.y1)
        val y2 = Math.min(r.y2, r2.y2)

        (x1, y1) -> (x2, y2)

    def area:Long = (r.x2.toLong - r.x1) * (r.y2.toLong - r.y1)
}

extension (e:Edge) {
    def minX = Math.min(e._1._1, e._2._1)
    def minY = Math.min(e._1._2, e._2._2)
    def maxX = Math.max(e._1._1, e._2._1)
    def maxY = Math.max(e._1._2, e._2._2)

    def inclusiveLength = e.maxY - e.minY + e.maxX - e.minX + 1
    def exclusiveLength = e.maxY - e.minY + e.maxX - e.minX 

    def isHorizontal:Boolean = 
        e._1._2 == e._2._2

}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val instructions = lines.map(decompose2)


    // We need to keep a set of edges.
    var cursor:Coord = (0, 0)
    var edges = mutable.Set.empty[Edge]
    for 
        (dir, dist, col) <- instructions
    do
        val next = cursor + (dir * dist)
        // println(s"From $cursor to $next in $dir")
        edges.add((cursor, next))
        cursor = next


    // println(edges)

    // We only care about the points at the ends of edges. As it's a loop, we can just take the "from" point for every edge, and that'll get them all
    val points = edges.map(_._1)

    // debugging
    val xAddresses = points.map(_._1).toSeq.sorted
    val yAddresses = points.map(_._2).toSeq.sorted

    // axis aligned bounds, for use in debug pretty-printing for small puzzles
    val minX = xAddresses.min
    val maxX = xAddresses.max
    val minY = yAddresses.min
    val maxY = yAddresses.max

    println(s"---($minX $minY) to ($maxX $maxY), with ${edges.size} edges")

    // All edges going south that intersect with a y-value.
    // Let's use south so we can be consistent on whether the end point is included or not
    def sedgesAt(y:Int) = 
        edges.filter({ (e) => 
            e.maxY > y && e.minY <= y  
        }).toSeq.sortBy(_.minX)
    
    // The length of the path itself is going to be one of our components
    def edgeLength = instructions.map(_._2).sum

    // We're going to look at whole blocks of y-lines at once, by taking y-regions in the range [y1, y2)
    // We can do this with a zip, but I've added on to the tail just in case we were missing anything about the last line (though I don't think it's necessary)
    val zippedYs = yAddresses.zip(yAddresses.tail) :+ (maxY, maxY + 1)

    val insides:Seq[Region] = (for (y1, y2) <- zippedYs yield {
        val verticals = sedgesAt(y1).toSeq.sortBy((e) => e.x1)

        // Use parity to get x-ranges bounded within the vertical edges. Go one sq down and in to avoid including any lines
        val verticallyContainedRegions = 
            for 
                ((e1, e2), i) <- (if verticals.nonEmpty then verticals.zip(verticals.tail) else Nil).zipWithIndex if i % 2 == 0
            yield
                (e1.x1, y1) -> (e2.x1 + 1, y2 + 1)

        verticallyContainedRegions
    }).flatten

    val dbl = insides.combinations(2).filter({ case Seq(a, b) => a.intersects(b) }).map({ case Seq(a, b) => a.intersect(b) })

    // println("in " + insides)
    // println("d " + dbl)

    // // For pretty-printing small maps
    // def pp() = 
    //     val ipoints = mutable.Map.empty[Coord, Int]
    //     for 
    //         e <- edges.toSeq
    //         x <- e.minX to e.maxX
    //         y <- e.minY to e.maxY
    //     do 
    //         ()
    //         //ipoints((x, y)) = ipoints.getOrElse((x, y), 0) + 1

    //     val innards = mutable.Set.empty[Coord]
    //     for 
    //         ((x1, y1), (x2, y2)) <- insides.toSeq
    //         x <- x1 until x2
    //         y <- y1 until y2
    //     do 
    //         ipoints((x, y)) = ipoints.getOrElse((x, y), 0) + 1

    //     // val dbl = mutable.Set.empty[Coord]
    //     // for 
    //     //     ((x1, y1), (x2, y2)) <- doubleCountedEdgeRegions.toSeq
    //     //     x <- x1 until x2
    //     //     y <- y1 until y2
    //     // do 
    //     //     dbl.add((x, y))


    //     println(s"---($minX $minY) to ($maxX $maxY)")
    //     for 
    //         y <- minY to maxY
    //     do     
    //         for x <- minX to maxX do
    //             print(ipoints.get((x, y)).getOrElse(" "))                
    //         println()


    // pp()
        
    // Area contained by the verticals
    val insideArea = insides.map(_.area).sum
    val dblArea = dbl.map(_.area).sum

    // Intersect between that and the edges
    // val dbl = doubleCountedEdgeRegions.map(_.area).sum

    println(s" Inside area $insideArea  dbl $dblArea  double counted dbl ==> ${insideArea - dblArea}")


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

