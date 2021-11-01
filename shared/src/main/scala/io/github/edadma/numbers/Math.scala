package io.github.edadma.numbers

import math._

object Math {

  def asinh(x: Double) = log(x + sqrt(x * x + 1))

  def acosh(x: Double) = log(x + sqrt(x * x - 1))

  def atanh(x: Double) = (log(1 + x) - log(1 - x)) / 2

}
