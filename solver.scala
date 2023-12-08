// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star13/solver.scala
// (or select the "star13" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec

val directions = "LR"

lazy val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(isPrime)

def isPrime(x: Int): Boolean =
  primes.takeWhile(p => p * p <= x).forall(x % _ != 0)

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
        if from.last == target.last then stepCount 
        else 
            if instructions.isEmpty() then 
                navigate(from, instructions0, target, stepCount)
            else
                val h = instructions.head
                
                val dir = directions.indexOf(h)
                val newNode:String = network(from)(dir)
                navigate(newNode, instructions.drop(1), target, stepCount + 1)

    val starts = network.keys.toSeq.filter(_.endsWith("A"))

    val loopLengths = for s <- starts yield navigate(s, instructions0, "ZZZ", 0)

    def factors(n:Int) = (2 until n).filter((p) => n % p == 0)

    def primeFactors(n:Int) = factors(n).filter(isPrime)

    val primesInLoopLengths = for 
        l <- loopLengths
        p <- primeFactors(l)
    yield
        println(s"Factor of $l is $p") 
        p

    val lcm = primesInLoopLengths.toSet.toSeq.map(_.toLong).product
    println(lcm)


    














    

