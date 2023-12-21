// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star35/solver.scala
// (or select the "star35" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

import scala.collection.mutable

extension [T] (map:Map[Coord, T]) {
    def minX = map.keys.map(_._1).min
    def minY = map.keys.map(_._1).min
    def maxX = map.keys.map(_._1).max
    def maxY = map.keys.map(_._1).max

    def rangeX = map.maxX + 1 - map.minX
    def rangeY = map.maxY + 1 - map.minY

    def step(p:Coord, d:Coord) = (p + d) % (map.rangeX, map.rangeY) 

}

// replicate a grid nine times
def nonicate[T](map:Map[Coord, T]):Map[Coord, T] = {
    val minX = map.keys.map(_._1).min
    val minY = map.keys.map(_._1).min
    val maxX = map.keys.map(_._1).max
    val maxY = map.keys.map(_._1).max
    val rangeX = maxX + 1 - minX
    val rangeY = maxY + 1 - minY

    (for 
        dx <- Seq(-rangeX, 0, rangeX)
        dy <- Seq(-rangeY, 0, rangeY)
        (x, y) <- map.keySet.toSeq
    yield
        (x + dx, y + dy) -> map((x, y))).toMap

}



class JellyFlood(map: Map[Coord, Char]) {
    val distance = mutable.Map.empty[(Int, Int), Int]

    val minX = map.minX
    val minY = map.minY
    val maxX = map.maxX
    val maxY = map.maxY
    val rangeX = map.rangeX
    val rangeY = map.rangeY

    val mod = (rangeX, rangeY)
    def step(p:Coord, d:Coord) = 
        var (x, y) = p + d
        while x < minX do x += rangeX
        while x > maxX do x -= rangeX

        while y < minY do y += rangeY
        while y > maxY do y -= rangeY

        (x, y)



    def allowedDirs[T](map:Map[Coord, T]) = (for 
        coord <- map.keySet
        dd = all.filter((d) => map.contains(step(coord, d)) && !(map(step(coord, d)) == '#'))
    yield coord -> dd).toMap

    val allowedDirections = allowedDirs(map)


    def flood(p:(Int, Int), dist:Int)(limit:Int):Unit = {
        val q = mutable.Queue(p -> dist)

        while !q.isEmpty do
            val (loc, d) = q.dequeue()
            val add = check(loc, d)
            // println(add)
            val filtered = for (p, v) <- add if q.find({ case (pp, vv) => pp == p && vv <= v}).isEmpty && v <= limit yield p -> v
            q.enqueueAll(filtered)
    }

    final def check(p:(Int, Int), dist:Int):Seq[(Coord, Int)] = {
        // println(s"Checking $p $dist")
        distance(p) = dist //distance.getOrElse(p, Seq.empty) :+ dist
        val (x, y) = p

        for {
            d <- allowedDirections(p) 
            pp = step(p, d) if !distance.get(pp).exists(_ <= dist + 1)
        } yield (step(p, d), dist + 1)
    }

    def maxDistance() = 
        distance.maxBy({ case ((x, y), d) => d })

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
    val lines = Source.fromFile("test.txt").getLines().toSeq

    val map:Map[Coord, Char] = (for 
        y <- lines.indices
        x <- lines(y).indices
    yield (x, y) -> lines(y)(x)).toMap

    val nMap = nonicate(map)



    
    val start = map.find((_, c) => c == 'S').map(_._1).get
    println(s"Start is $start")
    var cursor = start

    // for i <- 0 until 64 do
    //     cursor = cursor.flatMap { (p) => (for d <- allowedDirections(p) yield p + d).toSet }
    //     println(cursor.size)



    val j = JellyFlood(nMap)
    j.flood(start, 0)(200)//(Int.MaxValue)

    j.pp()

    def canReach(v:Int) = j.distance.count((_, x) => x <= v && (v - x) % 2 == 0)
    println(s"Can reach ${canReach(10)}")



    


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

