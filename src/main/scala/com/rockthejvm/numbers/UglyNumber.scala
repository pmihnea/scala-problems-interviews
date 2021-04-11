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

    /*
    ascList(7 , [1,3,8], []) =
    = ascL(7 , [3,8], [1]) =
    = ascL(7, [8] ,[3,1]) = concatReverse([7,3,1], [8]) = [1,3,7,8]
     */
    @tailrec
    def ascendingList(n: Int, remaining: List[Int], acc: List[Int]): List[Int] = {
      if (remaining.isEmpty) (n :: acc).reverse
      else if (remaining.head < n) ascendingList(n, remaining.tail, remaining.head :: acc)
      else if (remaining.head == n) concatReverse(acc, remaining) // remove duplicates
      else concatReverse(n :: acc, remaining)
    }

    @tailrec
    def multiply(remaining: List[Int], acc: List[Int]): List[Int] = {
      if (remaining.isEmpty) acc
      else {
        val h = ascendingList(remaining.head, acc, Nil)
        val m2 = if(2 * remaining.head >0) ascendingList(2 * remaining.head, h, Nil) else h
        val m3 = if(3 * remaining.head> 0 ) ascendingList(3 * remaining.head, m2, Nil) else m2
        val m5 = if(5 * remaining.head > 0) ascendingList(5 * remaining.head, m3, Nil) else m3
        multiply(remaining.tail, m5)
      }
    }

    @tailrec
    def concatReverse[T](remaining: List[T], acc: List[T]): List[T] = {
      if (remaining.isEmpty) acc
      else concatReverse(remaining.tail, remaining.head :: acc)
    }

    @tailrec
    def nthUglyRec(counter: Int, acc: List[Int]): Int =
      if (counter == index) {
        acc.drop(index - 1).head
      } else {
        // 2^a * 3^b * 5^c
        // {2,3,5}
        // {2,3,5} :: {2*2,3*2,5*2} :: {2*3,3*3,5*3} :: {2*5,3*5,5*5}
        nthUglyRec(counter + 1, multiply(acc, Nil))
      }

    nthUglyRec(1, List(1))
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
    println(s"nthUglyNumber($index) = ${nthUgly(index)
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
}
