package util

import scala.annotation.tailrec

// Random mathematical stuff from previous puzzles


/**
  * Returns the greatest common divisor (gcd) and the two Bezout coefficients for a and b 
  *
  * @param a
  * @param b
  * @return
  */
def extEuclid(a:Long, b:Long):(Long, Long, Long) = 

    @tailrec
    def extEuclidR(q:List[Long], r:List[Long], s:List[Long], t:List[Long]):(Long, Long, Long) =
        val qq = r(1) / r(0)
        val rr = r(1) % r(0)
        val ss = s(1) - qq * s(0)
        val tt = t(1) - qq * t(0) 

        if rr == 0 then ((rr :: r).find(_ != 0).get, s(0), t(0)) else extEuclidR(qq :: q, rr :: r, ss :: s, tt :: t)

    extEuclidR(Nil, List(a, b), List(1, 0), List(0, 1))

def gcd(a:Long, b:Long) = extEuclid(a, b)(0)

/** A lazy list of the Int primes */
lazy val primes:LazyList[Long] = LazyList.iterate(2L) { (n) => 
    var cursor = n + 1
    val subseq = primes.iterator.takeWhile(_ < Math.sqrt(n))
    while subseq.exists((d) => cursor % d == 0) do
        cursor = cursor + 1

    // println("new prime" + cursor)
    cursor
}

def isPrime(x: Long): Boolean =
  primes.takeWhile(p => p * p <= x).forall(x % _ != 0)

//def factors(n:Long) = Range.Long(2, n, 1).filter((p) => n % p == 0)

def primeFactors(n:Long):List[Long] = 
    @tailrec def nextFactor(n:Long, found:List[Long]):List[Long] = 
        if n == 1 then found else
            primes.takeWhile(p => p * p <= n).find(n % _ == 0) match {
                case Some(p) => 
                    val r = n / p 
                    // println(s"$p $r")
                    nextFactor(n / p, p :: found)
                case None =>
                    n :: found
            }

    nextFactor(n, Nil)

def compositeFactors(n:Long):Seq[Long] = 
    val pf = primeFactors(n)
    val sets = (1 to pf.length).flatMap(pf.combinations(_).toSeq)
    1 +: sets.map((s:Seq[Long]) => s.product)

def primesFactorsIn(s:Seq[Long]) = for 
    l <- s
    p <- primeFactors(l)
yield    
    p

def lcm(s:Seq[Long]) = primesFactorsIn(s).toSet.toSeq.product

def coprime(a:Long, b:Long) = 
    val (gcd, _, _) = extEuclid(a, b)
    gcd == 1