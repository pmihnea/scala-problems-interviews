package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesisProblems extends App {
  /*
  valid: (), ()(), (())
  not valid: )(
   */
  def hasValidParentheses(string: String): Boolean = {
    val OPEN = '('
    val CLOSE = ')'

    def checkRec(index: Int, openParenthesis: Int): Boolean = {
      if (index == string.length) openParenthesis == 0
      else {
        if (string.charAt(index) == OPEN) checkRec(index + 1, openParenthesis + 1)
        else if (string.charAt(index) == CLOSE) {
          if (openParenthesis == 0) false
          else checkRec(index + 1, openParenthesis - 1)
        } else checkRec(index + 1, openParenthesis) // ignore other chars
      }
    }

    checkRec(0, 0)
  }

  private val OPEN_CLOSE = "()"

  def testValidParentheses = {
    println(hasValidParentheses(OPEN_CLOSE)) //true
    println(hasValidParentheses(OPEN_CLOSE + OPEN_CLOSE)) //true
    println(hasValidParentheses("(" + OPEN_CLOSE + ")")) //true
    println(hasValidParentheses("(aaa(bbb)ccc(ddd)eee)")) //true
    println(hasValidParentheses(")(")) //false
    println(hasValidParentheses("(" + OPEN_CLOSE)) //false
  }

  /*
  n = 1 => List("()")
  n = 2 => List("()()","(())")
  n = 3 => List("()()()", "(())()", "()(())", "(()())", "((()))")
   */
  def generateAllValidParentheses(n: Int): List[String] = {
    @tailrec
    def genParensRec(nRemainingParens: Int, currentStrings: Set[String]): Set[String] = {
      if (nRemainingParens == 0) currentStrings
      else if (currentStrings.isEmpty) genParensRec(nRemainingParens - 1, Set(OPEN_CLOSE))
      else {
        val newStrings: Set[String] = currentStrings.flatMap(current =>
          (0 to current.length).map(i => {
            val split = current.splitAt(i)
            split._1 + OPEN_CLOSE + split._2
          }).toSet)

        genParensRec(nRemainingParens - 1, newStrings)
      }
    }

    genParensRec(n, Set[String]()).toList
  }

  def testGenerateAllValidParentheses = {
    println(generateAllValidParentheses(0).sorted.mkString(", "))
    println(generateAllValidParentheses(1).sorted.mkString(", "))
    println(generateAllValidParentheses(2).sorted.mkString(", "))
    println(generateAllValidParentheses(3).sorted.mkString(", "))
    println(generateAllValidParentheses(4).sorted.mkString(", "))
  }

  testGenerateAllValidParentheses
}
