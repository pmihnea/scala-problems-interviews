package com.rockthejvm.numbers


object ParseInteger extends App {

  object Token extends Enumeration {
    type Token = Value
    val Prefix, Sign, Digits, Suffix, Invalid = Value
  }

  import Token._

  case class State(n: Int, sign: Int, token: Token)

  /*
  Valid token sequences: Prefix? Sign? Digits Suffix?
   */
  def parseInteger(string: String): Int = {
    assert(string != null && string.nonEmpty && !string.isBlank)

    def addDigit(state: State, c: Char): State = {
      val res = state.n * 10 + c.asDigit
      if (res > 0) state.copy(n = res, token = Digits)
      else if (state.sign > 0) state.copy(n = Int.MaxValue, token = Invalid)
      else state.copy(n = Int.MinValue, token = Invalid)
    }

    val state = string.to(LazyList).foldLeft(State(0, 1, token = Prefix))((state, c) => {
      if (state.token == Prefix) {
        if (c.isSpaceChar) state
        else if (c == '+') state.copy(sign = 1, token = Sign)
        else if (c == '-') state.copy(sign = -1, token = Sign)
        else if (c.isDigit) addDigit(state, c)
        else state.copy(token = Invalid)
      } else if (state.token == Sign || state.token == Digits) {
        if (c.isDigit) addDigit(state, c)
        else state.copy(token = Suffix)
      } else state
    })

    if (state.n == Int.MaxValue || state.n == Int.MinValue) state.n
    else if (state.token == Invalid) throw new IllegalArgumentException
    else state.n * state.sign
  }

  def test(string: String) = {
    println("'" + string + "' => " + parseInteger(string))
  }

  test("1")
  test("123")
  test("+123")
  test("-123")

  test("   +456 a number of 3 chars")
  test("   -456 a number of 3 chars")

  test(s"${Int.MaxValue}")
  test(s"${Int.MaxValue}0")
  test(s"${Int.MinValue}")
  test(s"${Int.MinValue}0")

  test(" abc  +456") //invalid

}
