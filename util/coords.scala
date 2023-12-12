package util

type Coord = (Int, Int)

extension (c:Coord) {
    def +(c2:Coord) = 
        val (x, y) = c
        val (xx, yy) = c2
        (xx + x, yy + y)

    def inverse = (-c._1, -c._2)
}

val North = (0, -1)
val South = (0, 1)
val East = (1, 0)
val West = (-1, 0)
val all = Seq(North, South, East, West)
