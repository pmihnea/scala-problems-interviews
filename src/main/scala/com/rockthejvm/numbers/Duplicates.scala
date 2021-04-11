package com.rockthejvm.numbers

import scala.annotation.tailrec

object Duplicates extends App {
  //all numbers appear exactly twice, except one, find that one

  /*
  [1,2,1,2,4,7,5,6,5,4,6]
   */
  def duplicates(list: List[Int]): Int = {
    // Complexity: runtime O(N) , space O(N)
    @tailrec
    def findSingle(remaining: List[Int], occurrences: Map[Int, Int]): Int = {
      if (remaining.isEmpty) occurrences.find { case (k, v) => v == 1 }.get._1
      else {
        findSingle(remaining.tail, occurrences.updatedWith(remaining.head)(optionalValue => optionalValue.map(_ + 1).orElse(Some(1))))
      }
    }

    findSingle(list, Map[Int, Int]())
  }


  def test(list: List[Int]) = {
    println(s"duplicates(${list}) = " + duplicates(list))
  }

  test(List(1))
  test(List(1,2,1))
  test(List(1,2,3,2,1,4,5,5,4))
}
