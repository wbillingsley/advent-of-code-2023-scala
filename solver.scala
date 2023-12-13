// This is the solution for part 2
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star19/solver.scala
// (or select the "star20" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*

type Line = Seq[Int]

val integers = "(-?\\d+)".r
val decimals = "(-?\\d+)([.]\\d+)?".r

def allIntegersIn(s:String) = integers.findAllIn(s).map(_.toLong).toSeq
def allDecimalsIn(s:String) = decimals.findAllIn(s).map(_.toDouble).toSeq


@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val puzzleBuf = mutable.Buffer.empty[Seq[Seq[Char]]]
    var cursor = lines
    while (!cursor.isEmpty) do
        val puz = cursor.takeWhile(!_.isBlank()).map(_.toSeq)
        puzzleBuf.append(puz)
        cursor = cursor.drop(puz.length).dropWhile(_.isBlank())

    val puzzles = puzzleBuf.toSeq

    val transposed = puzzles.map(_.transpose)

    def verticalReflections[T](seq:Seq[Seq[T]]) = {
        for 
            y <- seq.indices if y != 0
            top = seq.take(y).reverse
            bottom = seq.drop(y)
            zipped = top.zip(bottom) if zipped.forall((a, b) => a == b)
        yield y
    }

    
    val puzzleReflections = 
        for 
            p <- puzzles
            vert = verticalReflections(p)
            horiz = verticalReflections(p.transpose)
        yield 
            val hScore = if horiz.isEmpty then 0 else horiz(0)
            val vScore = if vert.isEmpty then 0 else 100 * vert(0)
            hScore + vScore

    println(puzzleReflections.sum)


    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

