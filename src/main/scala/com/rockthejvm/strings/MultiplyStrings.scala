package com.rockthejvm.strings

import scala.annotation.tailrec
import scala.util.Random

object MultiplyStrings extends App {

  def sumUnit(x: Byte, y: Byte, carry: Byte): Byte = ((x + y + carry) % 10).toByte

  def sumTen(x: Byte, y: Byte, carry: Byte): Byte = ((x + y + carry) / 10).toByte

  def multiplyUnit(x: Byte, y: Byte, carry: Byte): Byte = ((x * y + carry) % 10).toByte

  def multiplyTen(x: Byte, y: Byte, carry: Byte): Byte = ((x * y + carry) / 10).toByte

  @tailrec
  def multiplyByTen(n: Int, list: List[Byte]): List[Byte] = {
    if(n == 0) list
    else multiplyByTen(n-1, 0::list)
  }

  //addition
  // 5 + 4 = 9
  // 7 + 8 = 15
  // 56 + 78 = 6 + 8 + 50 + 70 = 4 + 10 + 50 + 70
  // 1234 + 567 = 4+7 + 30+60 + 200+500 + 1000
  /*
  Sums list of digits with the units as heads
   */
  @tailrec
  def sumLists(a: List[Byte], b: List[Byte], acc: List[Byte], carry: Byte): List[Byte] = {
    if (a.isEmpty && b.isEmpty) {
      if (carry > 0) (carry :: acc).reverse
      else acc.reverse
    }
    else {
      val aHead: Byte = if (a.isEmpty) 0 else a.head
      val bHead: Byte = if (b.isEmpty) 0 else b.head
      val aTail = if (a.isEmpty) a else a.tail
      val bTail = if (b.isEmpty) b else b.tail
      sumLists(aTail, bTail, sumUnit(aHead, bHead, carry) :: acc, sumTen(aHead, bHead, carry))
    }
  }

  // multiplication
  // 5 x 6 = 30
  // 5 x 12 = 5 x 2 + 5 x 10 = 10 + 50 = 60
  // 5 x 1234567 = 5 x 7 + 5 x 60 + ...
  // 50 x 12 = 5 x (10 x 12)
  // 500 x 12 = 5 x 10 x 10 x 12
  // 43 x 12 = 3 x 12 + 40 x 12
  // 43 x 123456 = 3 x 123456 + 40 x 123456

  /*
  Multiplies a digit with a list of digits with the unit as head
   */
  @tailrec
  def multiplyDigit(x: Byte, b: List[Byte], acc: List[Byte], carry: Byte): List[Byte] = {
    if(x == 0) List(0)
    else if(b.isEmpty) {
      if (carry > 0) (carry :: acc).reverse
      else acc.reverse
    } else multiplyDigit(x, b.tail, multiplyUnit(x, b.head, carry) :: acc, multiplyTen(x, b.head, carry))
  }

  /*
  Multiplies list of digits with the units as heads
   */
  @tailrec
  def multiplyLists(aRemaining: List[Byte], b: List[Byte], acc: List[List[Byte]], rank: Int): List[Byte] = {
    if(aRemaining.isEmpty) acc.reduceLeft((s1,s2) => sumLists(s1,s2, List(), 0))
    else multiplyLists(aRemaining.tail, b, multiplyByTen(rank, multiplyDigit(aRemaining.head, b, List(), 0)) :: acc, rank + 1)
  }

  // multiply two numbers represented as strings of arbitrary length
  def multiplyStrings(a: String, b: String): String = {
    val aList = a.map(c => (c-'0').toByte).toList.reverse // first the lower units
    val bList = b.map(c => (c-'0').toByte).toList.reverse
    val resultList = multiplyLists(aList, bList, List(), 0)
    val result = resultList.reverse.mkString
    if(result.isEmpty || result.charAt(0) == '0') "0"
    else result
  }

  println("# sum lists tests")
  println(sumLists(List(1), List(4), List(), 0)); //5
  println(sumLists(List(1), List(4,3,2), List(), 0)); //5,3,2
  println(sumLists(List(1,2,3), List(4,5,6), List(), 0)); //5,7,9
  println(sumLists(List(5,6,7), List(5,8,9), List(), 0)); //0,5,7,1
  println(sumLists(List(5,6,7), List(5,8,9,3), List(), 0)); //0,5,7,4

  println("# multiply lists tests")
  println(multiplyLists(List(2), List(4), List(), 0)); //8
  println(multiplyLists(List(2), List(4,5), List(), 0)); //8,0,1
  println(multiplyLists(List(2), List(4,5,6), List(), 0)); //8,0,3,1
  println(multiplyLists(List(0,2), List(4), List(), 0)); //0,8
  println(multiplyLists(List(1,2), List(4), List(), 0)); //4,8
  println(multiplyLists(List(1,2), List(4,5), List(), 0)); //4,3,1,1
  println(multiplyLists(List(1,2,3,4), List(4,5,6,7), List(), 0)); //4,3,9,2,7,0,3,3

  def test(a: String, b: String) = {
    println(s"$a x $b = ${multiplyStrings(a,b)}")
  }

  println("# multiplication tests")
  test("2","3")  //6
  test("12","3") //36
  test("2","13") //26
  test("20","3") //60
  test("20","34") //680
  test("25","32") //800
  test("4321","7654") //33072934
  test("4321","0") //0
  test("0","4321") //0
  private val random = new Random()
  test(
    (1 to 20).map(_ => random.nextInt(10)).mkString,
    (1 to 20).map(_ => random.nextInt(10)).mkString
  )
}
