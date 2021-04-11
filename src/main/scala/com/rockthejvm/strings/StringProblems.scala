package com.rockthejvm.strings

object StringProblems extends App {
  def countCharacters(s: String): Map[Char, Int] = {
    s.foldLeft(Map[Char, Int]())((m, c) => m.updatedWith(c)(maybeValue => maybeValue.map(_ + 1).orElse(Some(1)))
    )
  }

  def testCountCharacters(s: String) = {
    println(s"countCharacters($s) = " + countCharacters(s))
  }

  def testSuiteCountCharacters = {
    testCountCharacters("")
    testCountCharacters("a")
    testCountCharacters("abc")
    testCountCharacters("abccbaef")
  }

  def checkAnagrams(sa: String, so: String): Boolean = {
    val saCounters = countCharacters(sa)
    val finalCounters = so.foldLeft(saCounters)((m, c) => m.updatedWith(c)(maybeCounter => {
      maybeCounter match {
        case None => Some(-1)
        case Some(1) => None
        case Some(i) => Some(i - 1)
      }
    }))
    finalCounters.isEmpty
  }

  def testCheckAnagrams(sa: String, so: String) = {
    println(s"checkAnagrams($sa, $so) = " + checkAnagrams(sa, so))
  }

  def testSuiteCheckAnagrams = {
    testCheckAnagrams("abcab", "bacba")
    testCheckAnagrams("abcab", "bacbac")
    testCheckAnagrams("Scala", "laSca")
  }

  testSuiteCheckAnagrams
}
