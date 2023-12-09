// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star17/solver.scala
// (or select the "star17" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec

import util.*

type Line = Seq[Int]

val number = raw"([-\d]+)".r

def decompose(line:String):Seq[Long] = 
    number.findAllIn(line).map(_.toLong).toSeq

def differentiate(s:Seq[Long]):Seq[Long] = 
    s.zip(s.tail).map((a, b) => b - a)


def differentiateToZeroes(s:Seq[Long]):Seq[Seq[Long]] = 
    var count = 0
    var cursor = List(s)
    while cursor.head.exists(_ != 0) do
        count = count + 1
        cursor = differentiate(cursor.head) :: cursor
    cursor

def nextValue(diffTable:Seq[Seq[Long]]):Long = {
    var cursor = 0L
    for i <- diffTable.indices.reverse do
        cursor = diffTable(i).head - cursor
    cursor
}

def prependHistory(diffTable:Seq[Seq[Long]]):Seq[Seq[Long]] = {
    var cursor = 0L
    println("Last " + diffTable.head)
    for i <- diffTable.indices yield
        val line = diffTable(i)
        cursor = line.head - cursor
        val r = cursor +: line
        r
}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val histories = lines.map(decompose)
    val differentiated = histories.map(differentiateToZeroes)

    val prepended = differentiated.map(prependHistory)

    val firsts = for p <- prepended yield p.last.head

    println("Total: " + firsts.sum)



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

