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
    //countCharacters(sa) == countCharacters(so)
    sa.sorted == so.sorted
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
