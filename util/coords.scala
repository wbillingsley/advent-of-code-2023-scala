package util

type Coord = (Int, Int)

extension (c:Coord) {
    def +(c2:Coord) = 
        val (x, y) = c
        val (xx, yy) = c2
        (xx + x, yy + y)

    def inverse = (-c._1, -c._2)

    def * (s:Int) = 
        (c._1 * s, c._2 * s)

    def % (m:Coord) = 
        val (x, y) = c
        val (mx, my) = m

        val rx = if x % mx < 0 then (x % mx) + mx else x % mx
        val ry = if y % my < 0 then (y % my) + my else y % my
        (rx, ry)
}

val North = (0, -1)
val South = (0, 1)
val East = (1, 0)
val West = (-1, 0)
val all = Seq(North, South, East, West)

extension [T] (map:Map[Coord, T]) {
    def minX = map.keys.map(_._1).min
    def minY = map.keys.map(_._1).min
    def maxX = map.keys.map(_._1).max
    def maxY = map.keys.map(_._1).max

    def rangeX = map.maxX + 1 - map.minX
    def rangeY = map.maxY + 1 - map.minY

    def step(p:Coord, d:Coord) = (p + d) % (map.rangeX, map.rangeY) 

}

    import scala.collection.mutable


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
        // Turn the modulo off again now we're doing diamonds
        // var (x, y) = p + d
        // while x < minX do x += rangeX
        // while x > maxX do x -= rangeX

        // while y < minY do y += rangeY
        // while y > maxY do y -= rangeY

        // (x, y)
        p + d



    def allowedDirs[T](map:Map[Coord, T]) = (for 
        coord <- map.keySet
        dd = all.filter((d) => map.contains(step(coord, d)) && !(map(step(coord, d)) == '#'))
    yield coord -> dd).toMap

    val allowedDirections = allowedDirs(map)



    def flood(p:(Int, Int), dist:Int)(limit:Int):JellyFlood = {
        val q = mutable.Queue(p -> dist)

        while !q.isEmpty do
            val (loc, d) = q.dequeue()
            val add = check(loc, d)
            // println(add)
            val filtered = for (p, v) <- add if q.find({ case (pp, vv) => pp == p && vv <= v}).isEmpty && v <= limit yield p -> v
            q.enqueueAll(filtered)

        this
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
