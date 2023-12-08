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
    def navigate(from:Seq[String], instructions:String, target:String, stepCount:Int = 0):Int = 
        if from.forall(_.last == target.last) then stepCount 
        else 
            if instructions.isEmpty() then 
                println(s"Looped with stepCount $stepCount")
                for s <- from do println(s)
                navigate(from, instructions0, target, stepCount)
            else
                val h = instructions.head
                
                val dir = directions.indexOf(h)
                val newNode = from.map((f) => network(f)(dir))
                navigate(newNode, instructions.drop(1), target, stepCount + 1)

    val start = network.keySet.toSeq.filter(_.endsWith("A"))
    println(navigate(start, instructions0, "ZZZ", 0))

    














    

