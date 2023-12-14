// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star-28/solver.scala
// (or select the "star-28" branch from GitHub)

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

    // Just one grid today
    val puzzle = lines.map(_.toSeq)

    val transposed = puzzle.transpose

    def roll(section:Seq[Char]):Seq[Char] = 
        val (os, dots) = section.partition(_ == 'O')
        os ++ dots

    def rollLine(l:Seq[Char]):Seq[Char] =
        val buf = mutable.Buffer.empty[Char]
        var remaining = l
        while remaining.nonEmpty do 
            val sec = remaining.takeWhile(_ != '#')
            val rolled = roll(sec)
            buf.append(rolled*)
            if sec.length == remaining.length then
                remaining = remaining.drop(sec.length)
            else 
                buf.append('#')
                remaining = remaining.drop(sec.length + 1)
        buf.toSeq

    def loadOnLine(l:Seq[Char]):Int = 
        val os = l.takeWhile(_ != '#').filter(_ == 'O')
        val loads = for (o, y) <- os.zipWithIndex yield l.length - y
        println(s"Calculated rock loads $loads")
        loads.sum

    val loads = for line <- transposed yield 
        val rolled = rollLine(line)
        val loads = for (o, y) <- rolled.zipWithIndex if o == 'O' yield line.length - y
        loads.sum



    println(rollLine("OO...O.#..O..O..".toSeq))

    println(loads.sum)

    

    

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

