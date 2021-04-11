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
    def findSingle(remaining: List[Int], occurrences: Set[Int]): Int = {
      if (remaining.isEmpty) occurrences.head
      else {
        val newOccurrences = if (occurrences.contains(remaining.head)) occurrences - remaining.head
        else occurrences + remaining.head
        findSingle(remaining.tail, newOccurrences)
      }
    }

    findSingle(list, Set[Int]())
  }


  def test(list: List[Int]) = {
    println(s"duplicates(" +
      s"${list.take(20)}" +
      s"${if(list.length>20) "..."}) = " + duplicates(list))
  }

  test(List(1))
  test(List(1,2,1))
  test(List(1,2,3,2,1,4,5,5,4))
  private val list: List[Int] = (1 to 10000).toList
  test(list ++ List(53465534) ++ list)
}
