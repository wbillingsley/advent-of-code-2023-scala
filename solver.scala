// This is the solution for part 1
// For the solution to part 2, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star18/solver.scala
// (or select the "star18" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec

import util.*

type Line = Seq[Int]

val number = raw"([-\d]+)".r

def decompose(line:String):Seq[Int] = 
    number.findAllIn(line).map(_.toInt).toSeq

def differentiate(s:Seq[Int]):Seq[Int] = 
    s.zip(s.tail).map((a, b) => b - a)


def differentiateToZeroes(s:Seq[Int]):(Int, Seq[Seq[Int]]) = 
    var count = 0
    var cursor = List(s)
    while !cursor.head.isEmpty && cursor.head.exists(_ != 0) do
        count = count + 1
        cursor = differentiate(cursor.head) :: cursor
    (count, cursor)

def nextValue(diffTable:Seq[Seq[Int]]):Int = {
    var cursor = 0
    for i <- diffTable.indices.reverse do
        cursor = diffTable(i).last + cursor
    cursor

}

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val histories = lines.map(decompose)
    val differentiated = histories.map(differentiateToZeroes)

    val nexts = for (s, table) <- differentiated yield nextValue(table)
    




    println(nexts.sum)



    // May be useful to have this to spot crashes if using watch
    println("Re-ran at: " + java.util.Date().toLocaleString())




    














    

