package com.rockthejvm.numbers

object LargestNumber extends App {
  /*
  List(10,2) => 210
  List(3, 30, 5, 9, 34) => 9534330
   */
  def largestNumber(numbers: List[Int]): String = {
    if (numbers.isEmpty) ""
    else {
      val largest = numbers.sortWith((a, b) => s"$a$b".compareTo(s"$b$a") >= 0).mkString
      if(largest.charAt(0) == '0') "0"
      else largest
    }
  }

  println(largestNumber(List(10,2)))
  println(largestNumber(List(3, 30, 5, 9, 34)))
  println(largestNumber(List(0,0,0)))
  println(largestNumber(List(1)))
  println(largestNumber(List()))

}
