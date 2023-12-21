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
