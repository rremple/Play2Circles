package models

import math.abs

object Scalar {

  val epsilon: Double = 0.0001

  def format(n1: Double) = "%.4f".format(n1)

  def eq(n1: Double, n2: Double) = abs(n1 - n2) < epsilon

  def veryClose(n1: Double, n2: Double) = abs(n1 - n2) < epsilon / 100

  def le(n1: Double, n2: Double) = n1 <= n2 + epsilon

  def ge(n1: Double, n2: Double) = epsilon + n1 >= n2

}
