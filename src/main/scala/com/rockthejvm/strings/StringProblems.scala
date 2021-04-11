package com.rockthejvm.strings

object StringProblems extends App {
  def countCharacters(s: String): Map[Char,Int] = {
    s.foldLeft(Map[Char,Int]())((m , c) => m.updatedWith(c)(maybeValue => maybeValue.map(_ + 1).orElse(Some(1)))
    )
  }

  def testCountCharacters(s: String) = {
    println(s"countCharacters($s) = " + countCharacters(s))
  }

  def _testSuiteCountCharacters = {
    testCountCharacters("")
    testCountCharacters("a")
    testCountCharacters("abc")
    testCountCharacters("abccbaef")
  }


}
