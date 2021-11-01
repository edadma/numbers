package io.github.edadma.numbers

import BigDecimalMath.decimal128._

trait ComplexBigDecimalIsFractional extends Fractional[ComplexBigDecimal] {

  def plus(x: ComplexBigDecimal, y: ComplexBigDecimal): ComplexBigDecimal = x + y

  def minus(x: ComplexBigDecimal, y: ComplexBigDecimal): ComplexBigDecimal = x - y

  def times(x: ComplexBigDecimal, y: ComplexBigDecimal): ComplexBigDecimal = x * y

  def div(x: ComplexBigDecimal, y: ComplexBigDecimal): ComplexBigDecimal = x / y

  def negate(x: ComplexBigDecimal): ComplexBigDecimal = -x

  def parseString(str: String): Option[ComplexBigDecimal] = {
    import ComplexBigDecimalIsFractional.COMPLEX_BIGDECIMAL

    str match {
      case COMPLEX_BIGDECIMAL(re, im) => Some(ComplexBigDecimal(BigDecimal(re), BigDecimal(im)))
      case _                          => None
    }
  }

  def fromInt(x: Int): ComplexBigDecimal = x

  def toInt(x: ComplexBigDecimal): Int = x.intValue

  def toLong(x: ComplexBigDecimal): Long = x.longValue

  def toFloat(x: ComplexBigDecimal): Float = x.floatValue

  def toDouble(x: ComplexBigDecimal): Double = x.doubleValue

  def compare(x: ComplexBigDecimal, y: ComplexBigDecimal): Int = sys.error("can't compare complex numbers")

}

object ComplexBigDecimalIsFractional {

  private val COMPLEX_BIGDECIMAL = """(-?(?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)[+-]((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?i""".r

  implicit object complexBigDecimalIsFractional extends ComplexBigDecimalIsFractional

}
