package com.rockthejvm.strings

import scala.annotation.tailrec

object CompareVersionNumbers extends App {
  /*
  1.0.3.4 < 1.1.0
  -1 : v1 < v2
  = : v1 == v2
  1 : v1 > v2
   */
  /*
  Complexity runtime O(max(L1,L2)), space O(L1 + L2)
   */
  def compareVersionNumbers(version1: String, version2: String): Int = {
    // split the version in list of numbers
    // compareLists the two lists starting from heads
    // in case of equality the longer is the bigger

    def version2ListOfNumbers(version: String): List[Int] = {
      version.split('.').map(_.toInt).toList
    }

    val v1List = version2ListOfNumbers(version1)
    val v2List = version2ListOfNumbers(version2)

    @tailrec
    def compareLists(v1List: List[Int], v2List: List[Int]): Int = {
      if (v1List.isEmpty && v2List.isEmpty) 0
      else if (v1List.isEmpty) {
        if (v2List.exists(_ != 0)) -1
        else 0
      }
      else if (v2List.isEmpty) {
        if (v1List.exists(_ != 0)) 1
        else 0
      }
      else if (v1List.head == v2List.head) compareLists(v1List.tail, v2List.tail)
      else Ordering[Int].compare(v1List.head, v2List.head)
    }

    compareLists(v1List, v2List)
  }

  println(compareVersionNumbers("0.9", "1.0.3.4")) //-1
  println(compareVersionNumbers("1.0.3.4", "1.1.0")) //-1
  println(compareVersionNumbers("1.1.0", "2.0")) //-1
  println(compareVersionNumbers("2.1", "2.01")) //0
  println(compareVersionNumbers("2.1.1", "2.1.0")) //1
  println(compareVersionNumbers("2.1.1", "2.1")) //1
  println(compareVersionNumbers("2.1.0.0", "2.1")) //0
  println(compareVersionNumbers("2.1", "2.1.0.0")) //0
}
