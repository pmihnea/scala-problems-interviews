package com.rockthejvm.strings

object ReverseWords extends App{
  /*
  "  Hello   world   " -> "world Hello"
   */
  def reverseWords(string: String): String = {
    string.split(' ').filter(_.nonEmpty).reverse.mkString(" ")
  }

  println(reverseWords("  Hello   world   "))
  println(reverseWords("  Hello   "))
  println(reverseWords("     "))
}
