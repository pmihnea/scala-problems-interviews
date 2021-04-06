package com.rockthejvm.numbers

import scala.util.Random

object ApproximatePi extends App {
  def approximatePi(nPoints: Int): Double = {
    val random = new Random(System.currentTimeMillis())
    val nInsideCirclePoints = (1 to nPoints).map(_ => {
      val x = random.nextDouble()
      val y = random.nextDouble()
      x * x + y * y
    }).count(distance => distance < 1)

    nInsideCirclePoints * 4.0 / nPoints
  }

  println(s"Reference: ${Math.PI}")
  println(approximatePi(1000))
  println(approximatePi(10000))
  println(approximatePi(100000))
  println(approximatePi(1000000))
  println(approximatePi(10000000))
  println(approximatePi(100000000))
}
