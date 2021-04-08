package com.rockthejvm.numbers

import scala.annotation.tailrec

object ReverseInteger extends App {
  def reverseInteger(number: Int): Int = {
    @tailrec
    def reverseRec(number: Int, result: Int): Int = {
      if (number == 0) result
      else {
        val q = number / 10
        val r = number % 10
        val newResult = result * 10 + r

        if ((number >= 0) != (newResult >= 0)) 0
        else reverseRec(q, newResult)
      }
    }

    if (number == Int.MinValue) 0
    if (number < 0) -reverseRec(-number, 0)
    else reverseRec(number, 0)
  }


  println("0 => " + reverseInteger(0))
  println("1 => " + reverseInteger(1))
  println("12 => " + reverseInteger(12))
  println("123 => " + reverseInteger(123))
  println("1230 => " + reverseInteger(1230))
  println("1234567890 => " + reverseInteger(1234567890))
  println(s"Int.MaxValue=${Int.MaxValue} => " + reverseInteger(Int.MaxValue))

  println("-123 => " + reverseInteger(-123))
  println(s"Int.MinValue=${Int.MinValue} => " + reverseInteger(Int.MinValue))
}
