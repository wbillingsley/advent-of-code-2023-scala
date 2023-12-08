// This is the solution for part 2
// For the solution to part 1, https://github.com/wbillingsley/advent-of-code-2023-scala/blob/star15/solver.scala
// (or select the "star15" branch from GitHub)

import scala.io.*
import scala.annotation.tailrec

val directions = "LR"

lazy val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(isPrime)

def isPrime(x: Long): Boolean =
  primes.takeWhile(p => p * p <= x).forall(x % _ != 0)

def factors(n:Long) = Range.Long(2, n, 1).filter((p) => n % p == 0)

def primeFactors(n:Long) = factors(n).filter(isPrime)

def primesFactorsIn(s:Seq[Long]) = for 
    l <- s
    p <- primeFactors(l)
yield    
    p

def lcm(s:Seq[Long]) = primesFactorsIn(s).toSet.toSeq.product

@tailrec
def extEuclidR(q:List[Long], r:List[Long], s:List[Long], t:List[Long]):(Long, Long, Long) =
    val qq = r(1) / r(0)
    val rr = r(1) % r(0)
    val ss = s(1) - qq * s(0)
    val tt = t(1) - qq * t(0) 

    if rr == 0 then (qq, s(0), t(0)) else extEuclidR(qq :: q, rr :: r, ss :: s, tt :: t)


def extEuclid(a:Long, b:Long) = extEuclidR(Nil, List(a, b), List(1, 0), List(0, 1))

// A step is only unique if all the subsequent steps are the same
// (any deviation in the instructions will cause the path to diverge)
// So we have to keep the current state of the instructions
type Step = (String, String)


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

        lazy val loopLength = loop.map(_.length - 1).getOrElse(-1)

        lazy val lengthBeforeLoop:Int = 
            steps.length - loopLength

        lazy val loop:Option[List[Step]] = 
            var found:Option[List[Step]] = None
            var cursor = steps
            while found.isEmpty && cursor.nonEmpty do
                if cursor.tail.contains(cursor.head) then 
                    found = Some(cursor)
            found

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


    // x == 19951 (mod 19953) and x == 20513 (mod 20515)
    val (x, y) = (15, 69)
    val (q, s, t) = extEuclid(x, y)
    println(s"$q $s $t")
    println(s * x + t * y)

    val startNodes = network.keys.toSeq.filter(_.endsWith("A"))
    val starts = for s <- startNodes yield Path(List(s -> instructions0))
    
    val loops = for s <- starts yield s.navigateUntilLooped

    val modulos = for l <- loops yield l.loopLength.toLong
    val zLengths = for l <- loops yield l.zLength.toLong

    println(s"Loop lengths are $modulos, with lcm ${lcm(modulos)}")

    for l <- modulos do 
        println(s"$l has prime factors ${primeFactors(l)}")

    println(s"Z-lengths are $zLengths, with lcm ${lcm(zLengths)}")

    for l <- zLengths do 
        println(s"$l has prime factors ${primeFactors(l)}")

    println(s"Lead in lengths = ${loops.map(_.lengthBeforeLoop)}")

    // s == z1 % l1 == z2 % l2 == z3 % l3

    /*
 
        Loop lengths are List(19953, 20515, 22201, 12087, 14895, 13209), with lcm 780238279154060265
        19953 has prime factors Vector(3, 739)
        20515 has prime factors Vector(5, 11, 373)
        22201 has prime factors Vector(149)
        12087 has prime factors Vector(3, 17, 79)
        14895 has prime factors Vector(3, 5, 331)
        13209 has prime factors Vector(3, 7, 17, 37)
        Z-lengths are List(19951, 20513, 22199, 12083, 14893, 13207), with lcm 12324145107121
        19951 has prime factors Vector(71, 281)
        20513 has prime factors Vector(73, 281)
        22199 has prime factors Vector(79, 281)
        12083 has prime factors Vector(43, 281)
        14893 has prime factors Vector(53, 281)
        13207 has prime factors Vector(47, 281)
        Lead in lengths = List(1, 1, 1, 1, 1, 1)

    */


    /*
      Pick two that have loop lengths that are pairwise co-prime

      x == 19951 (mod 19953) and x == 20513 (mod 20515)

      Chinese Remainder Theorem 

    */

    // Now, we're going to pick the first loop
    val loop = modulos.head
    val z = zLengths.head

    var guess = z.toLong
    val zipped = modulos.zip(zLengths)
    def correct(g:Long) = 
        zipped.forall((ll, zz) => guess % ll == zz)

    // while 
    //     println("Trying " + guess)
    //     !correct(guess)
    // do guess = loop + guess

    // println("The answer was " + guess )







    // And it worked.
    // Though if one of the loops had two nodes ending "Z" in it, it probably wouldn't have.


    














    

