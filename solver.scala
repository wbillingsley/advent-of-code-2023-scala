// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star45/solver.scala
// (or select the "star45" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable

def stepWithCondition(path:List[Coord], allowedDirections:Map[Coord, Seq[Coord]])(cond: List[Coord] => Boolean):Seq[List[Coord]] = {
    val found = mutable.Buffer.empty[List[Coord]]

    // @tailrec
    def step(path:List[Coord]):Unit = 
        val (p :: t) = path
        for d <- allowedDirections(p) if !path.contains(p + d) do 
            if (cond((p + d) :: path)) then
                found.append((p + d) :: path)
            else 
                step((p + d) :: path)

    step(path)
    found.toSeq

}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val maze:Seq[Seq[Char]] = lines.map(_.toSeq)
    val mazeMap = (for 
        y <- maze.indices
        x <- maze(y).indices
    yield (x, y) -> maze(y)(x)).toMap

    val start = (maze(0).indexOf('.'), 0)
    val target = (maze(maze.indices.last).indexOf('.'), maze.indices.last)

    val j0 = JellyFlood(mazeMap) // only because I happened to leave the map of allowable directions in there
    val junctions = (for (p, dirs) <- j0.allowedDirections.toSeq if mazeMap(p) != '#' && dirs.length > 2 yield p -> dirs).toMap
    val junctionPositions = junctions.keySet + start + target

    println("Junctions " + junctionPositions.size)

    val junctionMap = (for 
        j <- junctionPositions.toSeq 
        dirs = j0.allowedDirections(j)
    yield {
        j -> (for d <- dirs yield
            val neighbours = stepWithCondition(List(j + d, j), j0.allowedDirections) {
                case (p :: t) => junctionPositions.contains(p) || p == start || p == target
            }
            val path = neighbours(0)
            path.head -> (path.length - 1)
        )

    }).toMap


    // current junction, visited, length
    type JunctionPath = (Coord, Set[Coord], Int)

    val begin = List((start, 0))

    def jstep(jp:JunctionPath):Seq[JunctionPath] = {
        val (p, tail, cost) = jp
        for (next, d) <- junctionMap(p) if !tail.contains(next) yield
            (next, tail + p, cost + d)
    }

    // val allPaths = mutable.Buffer.empty[JunctionPath]
    var longest:Option[Int] = None
    var cursor:Seq[JunctionPath] = Seq((start, Set.empty, 0))
    println("Go!")
    while cursor.nonEmpty do 
        println(s"Processing ${cursor.length} paths")
        val atTarget = cursor.filter(_._1 == target)
        if atTarget.nonEmpty then
            val long = atTarget.map(_._3).max
            if longest.isEmpty || longest.get < long then
                println(s"New longest $long")
                longest = Some(long)
        
        cursor = cursor.flatMap(jstep)

    
    println(s"Done, longest was $longest")



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

