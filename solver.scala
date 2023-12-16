// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star32/solver.scala
// (or select the "star32" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec
import scala.collection.mutable

import util.*
import scala.collection.immutable.Queue

type Line = Seq[Int]

val integers = "(-?\\d+)".r
val decimals = "(-?\\d+)([.]\\d+)?".r

def allIntegersIn(s:String) = integers.findAllIn(s).map(_.toLong).toSeq
def allDecimalsIn(s:String) = decimals.findAllIn(s).map(_.toDouble).toSeq

type Beam = (Coord, Coord)

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq

    val puz:Seq[Seq[Char]] = lines.map(_.toSeq)
    var energised = Set.empty[Beam]

    def puzContains(p:Coord):Boolean = 
        val (x, y) = p
        puz.indices.contains(y) && puz(y).indices.contains(x)

    def charAt(coord:Coord):Char = 
        val (x, y) = coord
        puz(y)(x)

    val beams:Seq[Beam] = Seq((0, 0) -> East)
    def traverse() = 
        var b = beams
        while !b.isEmpty do 
            b.foreach { (beam) => energised = energised + beam }
            b = (b.flatMap { (p, dir) => 
                (charAt(p), dir) match {
                    case ('.', _) => 
                        Seq((p + dir, dir))

                    case ('/', East) => 
                        Seq((p + North, North))
                    case ('/', West) => 
                        Seq((p + South, South))
                    case ('/', North) => 
                        Seq((p + East, East))
                    case ('/', South) => 
                        Seq((p + West, West))

                    case ('\\', East) => 
                        Seq((p + South, South))
                    case ('\\', West) => 
                        Seq((p + North, North))
                    case ('\\', North) => 
                        Seq((p + West, West))
                    case ('\\', South) => 
                        Seq((p + East, East))

                    case ('-', East) => Seq(((p + dir), dir))
                    case ('-', West) => Seq(((p + dir), dir))
                    case ('|', North) => Seq(((p + dir), dir))
                    case ('|', South) => Seq(((p + dir), dir))

                    case ('-', North) => Seq(((p + East), East), ((p + West), West) )
                    case ('-', South) => Seq(((p + East), East), ((p + West), West) )
                    case ('|', East) => Seq(((p + North), North), ((p + South), South))
                    case ('|', West) => Seq(((p + North), North), ((p + South), South))
                }

            }).filter((beam) => !energised.contains(beam) && puzContains(beam._1))

    println("before")
    traverse()
    println(energised.map((beam, dir) => beam).size)





    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

