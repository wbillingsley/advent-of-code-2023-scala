// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star13/solver.scala
// (or select the "star13" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec

val directions = "LR"



@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val instructions0 = lines(0)

    val network = (for 
        line <- lines.drop(2) 
    yield 
        val s"$node = ($left, $right)" = line
        node -> Seq(left, right)).toMap

    @tailrec
    def navigate(from:String, instructions:String, target:String, stepCount:Int = 0):Int = 
        if from == target then stepCount 
        else 
            if instructions.isEmpty() then 
                navigate(from, instructions0, target, stepCount)
            else
                val h = instructions.head
                
                val dir = directions.indexOf(h)
                val newNode:String = network(from)(dir)
                navigate(newNode, instructions.drop(1), target, stepCount + 1)

    println(navigate("AAA", instructions0, "ZZZ", 0))

    














    

