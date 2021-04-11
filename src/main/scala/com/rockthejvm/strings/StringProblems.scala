package com.rockthejvm.strings

import scala.annotation.tailrec

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

  /*
    justify("abc abc ab abc ab ab abc", 12) = "abc abc ab |abc  ab  ab |abc"
     */
  def justify(text: String, width: Int): String = {
    assert(width > 2)
    // split the text in words
    val words = text.split(' ').toList

    // split the words in rows
    @tailrec
    def splitInRows(words: List[String], currentRow: List[String], currentRowLength: Int, result: List[List[String]]): List[List[String]] = {
      if (words.isEmpty) (currentRow.reverse :: result).reverse
      else if (currentRow.isEmpty)
        if (words.head.length > width) {
          val (before, after) = words.head.splitAt(width - 1)
          splitInRows(after :: words.tail, currentRow, currentRowLength, List(before + "-") :: result)
        } else {
          splitInRows(words.tail, words.head :: currentRow, words.head.length, result)
        }
      else if (currentRowLength + 1 + words.head.length > width) splitInRows(words, List(), 0, currentRow.reverse :: result)
      else splitInRows(words.tail, words.head :: currentRow, currentRowLength + 1 + words.head.length, result)
    }

    val rows = splitInRows(words, List(), 0, List())

    // distribute space between the words on each row so the words are equally far from each other and all rows have the given width
    def spaceWordsInRow(row: List[String], minSpacesBetweenWords: String, remainingSpacesBetweenWords: Int, acc: List[String]): String = {
      if (row.isEmpty) acc.reverse.mkString
      else if (acc.isEmpty) spaceWordsInRow(row.tail, minSpacesBetweenWords, remainingSpacesBetweenWords, row.head :: acc)
      else {
        val spaces = minSpacesBetweenWords + (if (remainingSpacesBetweenWords > 0) " " else "")
        val nSpacesLeft = if (remainingSpacesBetweenWords > 0) remainingSpacesBetweenWords - 1 else 0
        spaceWordsInRow(row.tail, minSpacesBetweenWords, nSpacesLeft,
          row.head :: spaces :: acc
        )
      }
    }

    val spacedRows = rows.filter(!_.isEmpty).map(row => {
      val rowWordsLength = row.foldLeft(0)(_ + _.length)
      val rowWordsCount = row.count(_ => true)
      val minSpacesBetweenWords = if (rowWordsCount <= 1) 0 else (width - rowWordsLength) / (rowWordsCount - 1)
      val remainingSpacesBetweenWords = if (rowWordsCount <= 1) 0 else (width - rowWordsLength) % (rowWordsCount - 1)

      val spaces = Array.fill(minSpacesBetweenWords)(" ").mkString
      spaceWordsInRow(row, spaces, remainingSpacesBetweenWords, List())
    })
    spacedRows.mkString("\n")
  }

  def testJustify = {
    println(justify("", 3))
    println("--------------")
    println(justify("1", 3))
    println("--------------")
    println(justify("1 2", 3))
    println("--------------")
    println(justify("1 2 3 4 5", 3))
    println("--------------")
    println(justify("1234 5678", 3))
    println("--------------")
    println(justify("123 234 345 456", 8))
    println("--------------")
    println(justify("abc def gh ijk lm op qrs uv w xz abc def gh ijk lm op qrs uv w xz 123 45 1234567890123 456", 12))
  }

  testJustify
}
