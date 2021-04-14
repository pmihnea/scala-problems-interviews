package com.rockthejvm.strings

object ReorganizeString extends App {

  def countChars(string: String): Map[Char, Int] = string.foldLeft(Map[Char, Int]()) {
    case (map, char) => map + (char -> (map.getOrElse(char, 0) + 1))
  }

  /*
  Complexity: runtime O(N^2) - for each element a max is calculated
   */
  def reorganizeRec(charCount: Map[Char, Int], lastChar: Char = '\u0000', result: String = ""): String = {
    if (charCount.isEmpty) result
    else {
      val newChar = charCount.filter(_._1 != lastChar).maxBy(_._2)._1
      val newCharCount =
        if (charCount(newChar) == 1) charCount - newChar
        else charCount + (newChar -> (charCount(newChar) - 1))
      reorganizeRec(newCharCount, newChar, result + newChar)
    }
  }

  // "aaabc" -> "abaca" or "acaba"
  // "aaa" -> ""
  // rearrange chars so that no two adjacent chars are identical
  def reorganizeString(string: String): String = {
    // count chars occurrences in a Map[Char,Int]
    // traverse the map and decrease the occurrence by one and add the char in the result
    // repeat the traversal until no char occurrence is left
    // if a char is repeatedly added to the result then return the empty result

    val charCount = countChars(string)
    if (charCount.values.exists(_ > (string.length + 1) / 2)) "" // impossible case
    else reorganizeRec(charCount)

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
