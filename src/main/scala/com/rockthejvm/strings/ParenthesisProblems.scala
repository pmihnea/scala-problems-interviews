package com.rockthejvm.strings

object ParenthesisProblems extends App{
  /*
  valid: (), ()(), (())
  not valid: )(
   */
  def hasValidParentheses(string: String): Boolean = {
    val OPEN = '('
    val CLOSE = ')'
    def checkRec(index: Int, openParenthesis: Int) :Boolean= {
      if(index == string.length) openParenthesis == 0
      else {
        if (string.charAt(index) == OPEN) checkRec(index + 1, openParenthesis + 1)
        else if (string.charAt(index) == CLOSE) {
          if(openParenthesis == 0) false
          else checkRec(index+1, openParenthesis - 1)
        } else checkRec(index+1, openParenthesis) // ignore other chars
      }
    }
    checkRec(0, 0)
  }

  def testValidParentheses = {
    println(hasValidParentheses("()")) //true
    println(hasValidParentheses("()()")) //true
    println(hasValidParentheses("(())")) //true
    println(hasValidParentheses("(aaa(bbb)ccc(ddd)eee)")) //true
    println(hasValidParentheses(")(")) //false
    println(hasValidParentheses("(()")) //false
  }
  testValidParentheses
}
