package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberProblems extends App {
  /*
  Complexity O(sqrt(N))
   */
  def isPrime(n: Int): Boolean = {
    val limit: Int = Math.sqrt(Math.abs(n)).intValue()

    @tailrec
    def isPrimeRec(current: Int): Boolean = {
      if (current > limit) true
      else if (n % current == 0) false
      else isPrimeRec(current + 1)
    }

    if (n <= 1 && n >= -1) false
    else isPrimeRec(2)
  }

  def isPrimeSieveOfEratosthenes(n: Int): Boolean = {

    /*
    Complexity: O(N) - 2xN once for traversing and once for reversing the result
     */
    @tailrec
    def removeMultiples(factor: Int, remaining: List[Int], result: List[Int]): List[Int] = {
      if (remaining.isEmpty) result.reverse //check if reverse is really needed
      else if (remaining.head % factor == 0) removeMultiples(factor, remaining.tail, result)
      else removeMultiples(factor, remaining.tail, remaining.head :: result)
    }

    /*
    Complexity: Is complex to calculate! O(N * log(log(N)) )
     */
    @tailrec
    def findAllPrimes(remaining: List[Int], result: List[Int]): List[Int] = {
      if (remaining.isEmpty) result
      else {
        val newRemaining = removeMultiples(remaining.head, remaining.tail, Nil)
        findAllPrimes(newRemaining, remaining.head :: result)
      }
    }

    if (n <= 1 && n >= -1) false
    else {
      val limit: Int = Math.sqrt(Math.abs(n)).intValue()
      val primes = findAllPrimes(List.from(2 to limit), Nil)
      primes.find(p => n % p == 0).isEmpty
    }
  }

  private def testIsPrime(numbers: List[Int]) = {
    println("  n - isPrime(n) - isPrimeSieveOfEratosthenes(n)")
    numbers.foreach(n => {
      val v1 = isPrime(n)
      val v2 = isPrimeSieveOfEratosthenes(n)
      println(f"$n%3d - $v1%5b - $v2%5b ->" + (if (v1 == v2) "OK" else "NOK"))
    })
    println()
  }

  testIsPrime(List.from(-100 to 100))
  testIsPrime(List(2,15,2003,2731189,517935872,1,0,-2003))

}
