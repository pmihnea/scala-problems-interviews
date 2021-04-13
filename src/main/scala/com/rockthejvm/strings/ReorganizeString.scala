package com.rockthejvm.strings

import scala.annotation.tailrec

object ReorganizeString extends App {

  def countChars(string: String): Map[Char, Int] = string.foldLeft(Map[Char, Int]()) {
    case (map, char) => map + (char -> (map.getOrElse(char, 0) + 1))
  }

  // "aaabc" -> "abaca" or "acaba"
  // "aaa" -> ""
  // rearrange chars so that no two adjacent chars are identical
  def reorganizeString(string: String): String = {
    // count chars occurrences in a Map[Char,Int]
    // traverse the map and decrease the occurrence by one and add the char in the result
    // repeat the traversal until no char occurrence is left
    // if a char is repeatedly added to the result then return the empty result

    @tailrec
    def reorganizeStringRec(remaining: List[(Char, Int)], acc: List[(Char, Int)], result: List[Char]): List[Char] = {
      if (remaining.isEmpty && acc.isEmpty) result
      else if (remaining.isEmpty) reorganizeStringRec(acc.reverse, List(), result)
      else {
        val (char, n) = remaining.head
        assert(n > 0)
        if (!result.isEmpty && result.head == char) List() //two identical adjacent chars
        else if (n > 1) reorganizeStringRec(remaining.tail, (char, n - 1) :: acc, char :: result)
        else reorganizeStringRec(remaining.tail, acc, char :: result)
      }
    }

    /*
    rsr([aaabc],[],[],false) = rsr([aabc],[],[a],true)
    = rsr([abc], [a], [a], true) = rsr([bc],[aa],[a],true)
    = rsr([c],[aa],[ba],true) = rsr([], [aa], [cba], true)
    = rsr([aa], [], [cba], false) =
     */
    def reorganizeStringRec2(remaining: List[Char], acc: List[Char], result: List[Char], changed: Boolean): List[Char] = {
      if (remaining.isEmpty && acc.isEmpty) result
      else if (remaining.isEmpty) {
        if (changed) reorganizeStringRec2(acc, List(), result, false)
        else List() // no changes -> not possible to find a next valid char
      }
      else {
        val nextChar = remaining.head
        if (result.isEmpty) reorganizeStringRec2(remaining.tail, acc, nextChar :: result, true)
        else if (nextChar == result.head) reorganizeStringRec2(remaining.tail, nextChar :: acc, result, changed)
        else reorganizeStringRec2(remaining.tail, acc, nextChar :: result, true)
      }
    }

    def permutations(string: String): LazyList[String] = {
      if (string.length == 1) LazyList(string)
      else {
        val char = string.substring(0, 1)
        val rest = string.substring(1)
        val restPermutations = permutations(rest)
        restPermutations.flatMap(p => {
          (0 to p.length).map(index => {
            val (before, after) = p.splitAt(index)
            s"$before$char$after"
          }).filter(_.nonEmpty)
        })
      }
    }

    def valid(string: String): Boolean = {
      if(string.length == 1) true
      else if(string.length == 2) string.charAt(0) != string.charAt(1)
      else {
        (1 to string.length-2).forall( index => string.charAt(index-1) != string.charAt(index) && string.charAt(index+1) != string.charAt(index) )
      }
    }
    permutations(string).filter(valid(_)).headOption.getOrElse("")
  }


  def test(string: String) = {
    val result = reorganizeString(string)
    println(s"${string.length}:[$string] -> ${result.length}:[$result]")
  }

  test("aaa")
  test("aab")
  test("baa")
  test("aaabc")
  test("aaabbbccc")
  test("aaaabbbccc")
}
