package com.rockthejvm.numbers

import scala.annotation.tailrec

object UglyNumber extends App {
  // ugly = only the factors 2,3,5
  // 1 is ugly
  @tailrec
  def uglyNumber(number: Int): Boolean = {
    assert(number > 0)
    if (number == 1) true
    else {
      if (number % 2 == 0) uglyNumber(number / 2)
      else if (number % 3 == 0) uglyNumber(number / 3)
      else if (number % 5 == 0) uglyNumber(number / 5)
      else false
    }
  }

  // the nth ugly number given the index
  // 1 is the first ugly number
  def nthUgly(index: Int): Int = {

    @tailrec
    def nthUglyRec(result: List[Int], counter: Int): Int = {
      if (counter == index) result.head // max element found
      else nthUglyRec(findNextLowerBoundedMin(result, result.head, Int.MaxValue) :: result, counter + 1)
    }

    /*
    fN([3,2,1], 3, MaxValue] -> {min(MaxValue, 3*2 > 3, 3*3 > 3, 3*5 > 3)=6}
    = fN([2,1], 3, 6) -> {min(6, 2*2 > 3, 2*3 > 3, 2*5 > 3)=4}
    = fN([1], 3, 4) -> {min(4, 1*2 <= 3, 1*2 <= 3, 1*5 > 5)=4
    = fN([], 3, 4) = 4
     */
    @tailrec
    def findNextLowerBoundedMin(remaining: List[Int], lowerBound: Int, currentMin: Int): Int = {
      if (remaining.isEmpty) currentMin
      else {
        val last = remaining.head
        val p2 = last * 2
        val p3 = last * 3
        val p5 = last * 5
        val n2 = if (p2 > lowerBound) p2 else currentMin
        val n3 = if (p3 > lowerBound) p3 else currentMin
        val n5 = if (p5 > lowerBound) p5 else currentMin
        val nextMin = Math.min(Math.min(currentMin, n2), Math.min(n3, n5))
        findNextLowerBoundedMin(remaining.tail, lowerBound, nextMin)
      }
    }

    nthUglyRec(List(1), 1)
  }

  def test(number: Int) = {
    println(s"uglyNumber($number) = ${
      uglyNumber(number)
    }")
  }

  println(uglyNumber(1))
  println(uglyNumber(2))
  println(uglyNumber(3))
  println(uglyNumber(5))
  println(uglyNumber(2 * 3 * 5))
  println(uglyNumber(2 * 2 * 3 * 2 * 5 * 5))
  println(uglyNumber(2 * 3 * 5 * 7))

  def testNth(index: Int) = {
    println(s"nthUglyNumber($index) = ${
      nthUgly(index)
    }")
  }

  testNth(1)
  testNth(2)
  testNth(3)
  testNth(4)
  testNth(5)
  testNth(6)
  testNth(7)
  testNth(8)
  testNth(9)
  testNth(10)
  testNth(11)
  testNth(12)
  testNth(13)
  testNth(14)
  testNth(15)
  testNth(16)
  testNth(17)
  testNth(18)
  testNth(19)
  testNth(20)

  testNth(200)
  testNth(2000)

}
