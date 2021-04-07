package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder

object RecurringDecimals extends App {

  case class DivisionResult(quotient: Byte, remainder: Int)

  import scala.collection.mutable.{Map => MMap}

  class ResultIndex() {
    val map = MMap[Byte, MMap[Int, DivisionResult]]()
    (0 to 9).foreach(d => map.put(d.toByte, MMap[Int, DivisionResult]()))

    def add(divisionResult: DivisionResult): Unit = {
      val quotientMap = map.get(divisionResult.quotient).getOrElse(MMap[Int, DivisionResult]())
      if(quotientMap.isEmpty) map.put(divisionResult.quotient, quotientMap)
      quotientMap.put(divisionResult.remainder, divisionResult)
    }

    def find(divisionResult: DivisionResult): Option[DivisionResult] = {
      map.get(divisionResult.quotient).flatMap(quotientMap =>
        quotientMap.get(divisionResult.remainder))
    }
  }

  @tailrec
  def findDecimals(numerator: Int, denominator: Int, results: List[DivisionResult], resultIndex: ResultIndex): (List[DivisionResult], Option[DivisionResult]) = {
    val quotient = (numerator / denominator).toByte
    val remainder = numerator % denominator
    val result = DivisionResult(quotient, remainder)
    if (remainder == 0) {
      ((result :: results).reverse, None)
    } else {
      val maybeResult =  resultIndex.find(result) //results.find(_.equals(result))
      if (maybeResult.isDefined) {
        (results.reverse, maybeResult)
      } else {
        resultIndex.add(result)
        findDecimals(remainder * 10, denominator, result :: results, resultIndex)
      }
    }
  }

  def fractionToRecurringDecimals(numerator: Int, denominator: Int): String = {
    val stringBuilder = new StringBuilder()
    if (numerator < 0 && denominator > 0 || numerator > 0 && denominator < 0) {
      stringBuilder.append("-")
    }
    val quotient = Math.abs(numerator / denominator)
    val remainder = Math.abs(numerator % denominator)
    val (decimals, maybeFirstRecurringDivisionResult) = findDecimals(remainder * 10, Math.abs(denominator), Nil, new ResultIndex())
    stringBuilder.append(quotient)
    if (remainder != 0) stringBuilder.append(".")
    maybeFirstRecurringDivisionResult match {
      case Some(firstRecurringDivisionResult) => {
        decimals.foldLeft(stringBuilder)((sb: StringBuilder, divisionResult: DivisionResult) => {
          if (divisionResult.equals(firstRecurringDivisionResult)) sb.append("(")
          sb.append(divisionResult.quotient)
        }).append(")")
      }
      case None => decimals.foldLeft(stringBuilder)((sb: StringBuilder, divisionResult: DivisionResult) => {
        sb.append(divisionResult.quotient)
      })
    }
    if(stringBuilder.length() > 100) s"{... ${stringBuilder.length()} digits ...}"
    else stringBuilder.toString()
  }

  println("1/5=" + fractionToRecurringDecimals(1, 5))
  println("1/50=" + fractionToRecurringDecimals(1, 50))
  println("1/3=" + fractionToRecurringDecimals(1, 3))
  println("1/6=" + fractionToRecurringDecimals(1, 6))
  println("1/333=" + fractionToRecurringDecimals(1, 333))
  println("1/7=" + fractionToRecurringDecimals(1, 7))
  println("1/2003=" + fractionToRecurringDecimals(1, 2003))
  println("-1/2=" + fractionToRecurringDecimals(-1, 2))
  println("1/Int.MinValue>>4=" + fractionToRecurringDecimals(1, Int.MinValue>>4))
  println("1/1987=" + fractionToRecurringDecimals(1, 1987))
  println("1/7919=" + fractionToRecurringDecimals(1, 7919))
  println("1/16127=" + fractionToRecurringDecimals(1, 16127))
  println("1/1046527=" + fractionToRecurringDecimals(1, 1046527))
  println("1/Int.MaxValue>>4=" + fractionToRecurringDecimals(1, Int.MaxValue>>4))

}
