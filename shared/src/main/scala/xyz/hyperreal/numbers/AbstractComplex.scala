package xyz.hyperreal.numbers

import math._

abstract class AbstractComplex[T: Numeric, C <: Complex[T, Double, C, ComplexDouble]] extends Complex[T, Double, C, ComplexDouble] {

  protected def promote(re: Double, im: Double): ComplexDouble = ComplexDouble(re, im)

  protected def _floor(a: Double): Double = math.floor(a)

  protected def _ceil(a: Double): Double = math.ceil(a)

  protected def _sqrt(a: Double): Double = math.sqrt(a)

  protected def _atan2(y: Double, x: Double): Double = atan2(y, x)

  protected def _ln(a: Double): Double = log(a)

  protected def _exp(a: Double): Double = math.exp(a)

  protected def _sin(a: Double): Double = math.sin(a)

  protected def _cos(a: Double): Double = math.cos(a)

  protected def _pow(a: Double, b: Double): Double = math.pow(a, b)

}
