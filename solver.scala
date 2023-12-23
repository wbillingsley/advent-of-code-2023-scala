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

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val maze:Seq[Seq[Char]] = lines.map(_.toSeq)
    val mazeMap = (for 
        y <- maze.indices
        x <- maze(y).indices
    yield (x, y) -> maze(y)(x)).toMap

    val start = (maze(0).indexOf('.'), 0)
    val target = (maze(maze.indices.last).indexOf('.'), maze.indices.last)

    val j = LongJellyFlood(mazeMap)
    println("About to flood")
    j.flood((start, Set.empty))
    println("Finished flood")

    val longest = j.seen.filter(_._1 == target).map((p, t) => t.size + 1).max

    println(s"Longest is $longest")

    println(start)





    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

