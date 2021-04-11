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
    def nthUglyRec(number: Int, counter: Int): Int = {
      if (uglyNumber(number)) {
        if (counter == index) number
        else nthUglyRec(number + 1, counter + 1)
      } else {
        nthUglyRec(number + 1, counter)
      }
    }

    nthUglyRec(1, 1)
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
  println(uglyNumber(16200))

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
  testNth(1600)

}
