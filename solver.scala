// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star15/solver.scala
// (or select the "star15" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec

import util.*

val directions = "LR"





// A step is only unique if all the subsequent steps are the same
// (any deviation in the instructions will cause the path to diverge)
// So we have to keep the current state of the instructions
type Step = (String, String)


// Solve x = a1 (mod n1) = a2 (mod n2) exhaustively
def sieve(a1:Long, n1:Long, a2:Long, n2:Long):Long = 
    println(s"Solving x = $a1 (mod $n1) = $a2 (mod $n2)")
    if coprime(n1, n2) then 
        println("Coprime! Using constructive existence proof")
        val (q, m1, m2) = extEuclid(n1, n2)
        val r = a1 * m2 * n2 + a2 * m1 * n1
        println(s"Got $r")
        r
    else 
        throw IllegalArgumentException("There's not necessarily a solution in the non-coprime case")

def solveCongruences(seq:Seq[(Long, Long)]) = 
    seq.reduce { case ((a1, n1), (a2, n2)) => 
        val x = sieve(a1, n1, a2, n2)
        
        (x, n1 * n2)
    }

@main def main() = 
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val instructions0 = lines(0)

    val network = (for 
        line <- lines.drop(2) 
    yield 
        val s"$node = ($left, $right)" = line
        node -> Seq(left, right)).toMap


    case class Path(steps:List[Step]) {

        def step = 
            val (from, instructions) = steps.head
            val dir = directions.indexOf(instructions.head)
            val nextNode = network(from)(dir)
            Path((nextNode, instructions.drop(1) + instructions.take(1)) :: steps)

        def hasLooped = steps.nonEmpty && steps.tail.contains(steps.head)

        lazy val loopLength = loop._2

        lazy val lengthBeforeLoop:Int = 
            steps.length - loopLength

        lazy val loop:(Int, Int) = 
            (0, steps.tail.indexOf(steps.head) + 1)
            

        @tailrec
        final def navigateUntil(p:Path => Boolean):Path = {
            if (p(this)) then 
                this
            else                 
                step.navigateUntil(p)
        }

        def navigateUntilLooped = navigateUntil(_.hasLooped)

        lazy val zLength = steps.reverseIterator.indexWhere(_._1.endsWith("Z"))

    }


    val startNodes = network.keys.toSeq.filter(_.endsWith("A"))
    val starts = for s <- startNodes yield Path(List(s -> instructions0))
    
    val loops = for s <- starts yield s.navigateUntilLooped

    val modulos = for l <- loops yield l.loopLength.toLong
    val zLengths = for l <- loops yield l.zLength.toLong

    println(s"Loop lengths are $modulos, with lcm ${lcm(modulos)}")

    for l <- modulos do 
        println(s"$l has prime factors ${primeFactors(l)}")

    println(s"Z-lengths are $zLengths, with lcm ${lcm(zLengths)}")

    // Ok, at this point, it should be apparent that the loop lengths and the z-lengths are identical.
    // The loop lengths (if you take their factors) also have the length of the instructions as a factor
    // This all looks carefully designed so that each path with land on Z with a constant period (including the first time)

    // For a while, I thought I was "lucky" that LCM worked, as it doesn't necessarily have to work for any P-shaped path,
    // But actually it looks like the puzzlers had to set it up this way in order to make it tractably solvable.
    // The instruction length has to be a factor of the loop length (as the state of a step includes the state of its instructions)
    // That means the loop lengths cannot be coprime
    // That means CRT-based solutions to solve x = a1 (mod n1) and x = a2 (mod n2) will become unwieldy
    // So as puzzle-setters, they effectively had to make a1, a2, etc all = 0
    // In which case, LCM(n1, n2, ...) has to work.



    














    

