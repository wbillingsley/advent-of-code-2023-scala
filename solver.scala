// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star46/solver.scala
// (or select the "star46" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable


class LongJellyFlood(map: Map[Coord, Char]) {
    val distance = mutable.Map.empty[(Int, Int), Int]

    val minX = map.minX
    val minY = map.minY
    val maxX = map.maxX
    val maxY = map.maxY
    val rangeX = map.rangeX
    val rangeY = map.rangeY

    val mod = (rangeX, rangeY)
    def step(p:Coord, d:Coord) = 
        p + d



    def allowedDirs(map:Map[Coord, Char]):Map[Coord, Seq[Coord]] = (for 
        coord <- map.keySet if map(coord) != '#'
        dd = all.filter((d) => map.contains(step(coord, d)) && map(step(coord, d)) != '#')
    yield 
        coord -> (map(coord) match {
            case '.' => dd
            case '>' => Seq(East)
            case '<' => Seq(West)
            case 'v' => Seq(South)
        })
    ).toMap

    val allowedDirections = allowedDirs(map)



    def flood(path:List[Coord], dist:Int)(limit:Int):LongJellyFlood = {
        val q = mutable.Queue(path -> dist)

        println("Begin flood for " + path)

        while !q.isEmpty do
            val (path, d) = q.dequeue()
            // println(s"Stepping $path for $d")
            val add = check(path, d)
            // println(add)
            // val filtered = for (pp, v) <- add if q.find({ case (pp, vv) => pp == p && vv <= v}).isEmpty && v <= limit yield p -> v
            q.enqueueAll(add)

        this
    }

    final def check(path:List[Coord], dist:Int):Seq[(List[Coord], Int)] = {
        // println(s"Checking $p $dist")
        val p :: tail = path 
        distance(p) = dist //distance.getOrElse(p, Seq.empty) :+ dist
        val (x, y) = p

        for {
            d <- allowedDirections(p) 
            pp = step(p, d) if !tail.contains(pp) && !distance.get(pp).exists(_ > dist + 1)
        } yield (pp :: path, dist + 1)
    }

    def maxDistance() = 
        distance.maxBy({ case ((x, y), d) => d })

    def parityDistance(n:Long) = 
        distance.values.count((d) => n >= d && (n - d) % 2 == 0)

    def pp() = {
        val mx = maxY
        val my = maxY
        for y <- minY to my do
            for x <- minX to mx do
                if map.get((x, y)).contains('#') then 
                    print(" ## ") 
                else
                    if distance.contains((x, y)) then print(f" ${distance((x, y))}%02d ") else print("    ")
            println()
    }

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
    j.flood(List(start), 0)(10000)
    println("Finished flood")

    j.pp()
    //println(j.distance(target))

    println(start)





    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

