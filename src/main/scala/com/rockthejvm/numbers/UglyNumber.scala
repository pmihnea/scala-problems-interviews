package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

  def min3(a: Int, b: Int, c: Int) =
    if (a <= b)
      if (a <= c) a
      else c
    else if (b <= c) b
    else c

  // the nth ugly number given the index
  // 1 is the first ugly number
  def nthUgly(index: Int): Int = {
    /*
    1 , 2, 3, 4, 5, 6, 8, 9, 10
    [12,16,18,20]
    [12,15,18,24,27,30]
    [15,20,25,30,40,45,50]
     */
    /*
  Complexity: runtime O(N), space O(N)
  */
    @tailrec
    def nthUglyRec(counter: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int]): Int = {
      val min = min3(q2.head, q3.head, q5.head)
      if (counter == index) min
      else {
        val newQ2 = (if (q2.head == min) q2.tail else q2).enqueue(min * 2)
        val newQ3 = (if (q3.head == min) q3.tail else q3).enqueue(min * 3)
        val newQ5 = (if (q5.head == min) q5.tail else q5).enqueue(min * 5)
        nthUglyRec(counter + 1, newQ2, newQ3, newQ5)
      }
    }

    if (index == 1) 1
    else nthUglyRec(1, Queue(2), Queue(3), Queue(5))
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
  testNth(1690)
  testNth(1691) //overflows

}
