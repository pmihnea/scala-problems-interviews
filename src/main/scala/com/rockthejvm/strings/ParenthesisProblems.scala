package com.rockthejvm.strings

object ParenthesisProblems extends App{
  /*
  valid: (), ()(), (())
  not valid: )(
   */
  def hasValidParentheses(string: String): Boolean = {
    val OPEN = '('
    val CLOSE = ')'
    def checkRec(index: Int, stack: List[Int]) :Boolean= {
      if(index == string.length) stack.isEmpty
      else {
        if (string.charAt(index) == OPEN) checkRec(index + 1, OPEN :: stack)
        else if (string.charAt(index) == CLOSE) {
          if(stack.isEmpty || stack.head != OPEN) false
          else checkRec(index+1, stack.tail)
        } else checkRec(index+1, stack)
      }
    }
    checkRec(0, Nil)
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
