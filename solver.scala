// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star43/solver.scala
// (or select the "star43" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable


class LongJellyFlood(map: Map[Coord, Char]) {
    // Every step must encompass one new point. The length of a path is just the number of points in it
    val seen = mutable.Set.empty[(Coord, Set[Coord])]

    val minX = map.minX
    val minY = map.minY
    val maxX = map.maxX
    val maxY = map.maxY
    val rangeX = map.rangeX
    val rangeY = map.rangeY

    def step(p:Coord, d:Coord) = 
        p + d

    def allowedDirs(map:Map[Coord, Char]):Map[Coord, Seq[Coord]] = (for 
        coord <- map.keySet if map(coord) != '#'
        dd = all.filter((d) => map.contains(step(coord, d)) && map(step(coord, d)) != '#')
    yield 
        coord -> (map(coord) match {
            case '.' => dd
            case '>' => dd // Seq(East)
            case '<' => dd //Seq(West)
            case 'v' => dd // Seq(South)
        })
    ).toMap

    val allowedDirections = allowedDirs(map)



    def flood(path:(Coord, Set[Coord])):LongJellyFlood = {
        val q = mutable.Queue(path)

        println("Begin flood for " + path)

        while !q.isEmpty do
            val (path, d) = q.dequeue()
            // println(s"Stepping $path for $d")
            val add = check(path, d)
            // println(add)
            // val filtered = for (pp, v) <- add if q.find({ case (pp, vv) => pp == p && vv <= v}).isEmpty && v <= limit yield p -> v

            // tried but wrong
            // val filtered = for ((p :: tail), v) <- add if q.find({ case (pp :: tt, vv) => pp == p && vv >= v}).isEmpty yield ((p :: tail), v)
            // q.enqueueAll(filtered)


            q.enqueueAll(add)

        this
    }

    final def check(path:(Coord, Set[Coord])):Seq[(Coord, Set[Coord])] = {
        // println(s"Checking $p $dist")
        val (p, tail) = path 

        for {
            d <- allowedDirections(p) 
            pp = step(p, d) if !seen.contains(pp, tail + p)
        } yield pp -> (tail + p)
    }

    // def pp() = {
    //     val mx = maxY
    //     val my = maxY
    //     for y <- minY to my do
    //         for x <- minX to mx do
    //             if map.get((x, y)).contains('#') then 
    //                 print(" ## ") 
    //             else
    //                 if distance.contains((x, y)) then print(f" ${distance((x, y))}%02d ") else print("    ")
    //         println()
    // }

}

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

    val j0 = JellyFlood(mazeMap)
    val junctions = (for (p, dirs) <- j0.allowedDirections.toSeq if dirs.length > 2 yield p -> dirs).toMap
    val junctionPositions = junctions.keySet + start + target

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

    val allPaths = mutable.Buffer.empty[JunctionPath]
    var cursor:Seq[JunctionPath] = Seq((start, Set.empty, 0))
    println("Go!")
    while cursor.nonEmpty do 
        val collected = allPaths.length
        println(collected)
        allPaths.append(cursor*)
        cursor = cursor.flatMap(jstep)

    val toTarget = allPaths.filter(_._1 == target).map(_._3)
    println(toTarget.max)



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

