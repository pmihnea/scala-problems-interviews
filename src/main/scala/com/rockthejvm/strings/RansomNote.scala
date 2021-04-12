package com.rockthejvm.strings

object RansomNote extends App {

  def ransomNode(note: String, magazine: String): Boolean = {
    def countChars(string: String): Map[Char, Int] = {
      string.foldLeft(Map[Char, Int]())((m, c) => m.updatedWith(c)(maybeCounter => maybeCounter.map(_ + 1).orElse(Some(1))))
    }

    val noteCounters: Map[Char, Int] = countChars(note)
    val magazineCounters: Map[Char, Int] = countChars(magazine)
    noteCounters.keySet.forall(char => magazineCounters.getOrElse(char,0) >= noteCounters.getOrElse(char,0))
/*
    val maybeFalse = noteCounters.flatMap { case (noteChar, noteCounter) =>
      magazineCounters.get(noteChar).map(magazineCounter =>
        magazineCounter >= noteCounter).orElse(Some(false))
    }.find(p => !p)
    maybeFalse.isEmpty
*/
  }

  def testRansomNote(note: String, magazine: String) = {
    println(s"note='$note'")
    println(s"magazine='$magazine'")
    println("note taken from magazine = " + ransomNode(note, magazine))
    println()
  }

  testRansomNote("abc de", "abcde abcde ")
  testRansomNote("abc de", "abcde")
}
