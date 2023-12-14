// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star27/solver.scala
// (or select the "star27" branch from GitHub)

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
    val puzzle:Seq[Seq[Char]] = lines.map(_.toSeq)

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

    def north(p:Seq[Seq[Char]]) = 
        val pp = p.transpose
        val ppp = for l <- pp yield rollLine(l)
        ppp.transpose

    def west(p:Seq[Seq[Char]]) = 
        for l <- p yield rollLine(l)

    def south(p:Seq[Seq[Char]]) = 
        val pp = p.transpose.map(_.reverse)
        val ppp = for l <- pp yield rollLine(l)
        ppp.map(_.reverse).transpose

    def east(p:Seq[Seq[Char]]) = 
        val pp = p.map(_.reverse)
        val ppp = for l <- pp yield rollLine(l)
        ppp.map(_.reverse)

    def loads(s:Seq[Seq[Char]]) = 
        val p = s.transpose
        (for 
            line <- p
            (o, y) <- line.zipWithIndex if o == 'O'
        yield line.length - y).sum
        

    // val loads = for line <- transposed yield 
    //     val rolled = rollLine(line)
    //     val loads = for (o, y) <- rolled.zipWithIndex if o == 'O' yield line.length - y
    //     loads.sum


    //println(rollLine("OO...O.#..O..O..".toSeq))

    println(s"North loads is ${loads(north(puzzle))}")

    def pp(p:Seq[Seq[Char]]) = 
        for line <- p do println(line.mkString)

    def cycle(p:Seq[Seq[Char]]) = 
        val n = north(p)
        // println("North")
        // pp(n)
        val w = west(n)
        // println("West")
        // pp(w)
        val s = south(w)
        // println("South")
        // pp(s)
        val e = east(s)
        // println("East")
        // pp(e)
        e

    val c = cycle(cycle(cycle(puzzle)))
    println("----")
    pp(c)

    var p = puzzle
    val loadsBuffer = mutable.Buffer.empty[Int]

    // Just do a few 
    for i <- 1 to 100 do
        p = cycle(p)
        val l = loads(p)
        println(s"Rotation $i loads ${loads(p)}")
        loadsBuffer.append(l)

    def cycled(s:Seq[Int]):Option[Seq[Int]] = 
        (1 until s.length / 4).find((n) =>            
            s.take(n) == s.drop(n).take(n)
        ).map(s.take(_))
    
    var cyc:Option[Seq[Int]] = None

    while 
        cyc = cycled(loadsBuffer.toSeq.reverse)
        cyc.isEmpty
    do 
        p = cycle(p)
        val l = loads(p)
        println(s"Rotation ${loadsBuffer.length} loads ${loads(p)}")
        loadsBuffer.append(l)

    println(s"cycled - ${loadsBuffer.length} rotations, cycle length ${cyc.get.length}, ${cyc.get} ")
    val last = loadsBuffer.length
    val entry = (1000000000 - last) % cyc.get.length
    println("Result " + cyc.get.reverse(entry - 1))

    

    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

