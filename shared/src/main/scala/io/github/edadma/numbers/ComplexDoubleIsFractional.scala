package io.github.edadma.numbers

trait ComplexDoubleIsFractional extends Fractional[ComplexDouble] {

  def plus(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x + y

  def minus(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x - y

  def times(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x * y

  def div(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x / y

  def negate(x: ComplexDouble): ComplexDouble = -x

  def parseString(str: String): Option[ComplexDouble] = {
    import ComplexDoubleIsFractional.{PURE_IMAGINARY, FULL_COMPLEX}

    val trimmed = str.trim

    trimmed match {
      case PURE_IMAGINARY(sign, coeff) =>
        val coefficient = if (coeff.isEmpty) 1.0 else coeff.toDouble
        val imaginary = if (sign == "-") -coefficient else coefficient
        Some(ComplexDouble(0, imaginary))

      case FULL_COMPLEX(real, sign, imagCoeff) =>
        val realPart = real.toDouble
        val coefficient = if (imagCoeff.isEmpty) 1.0 else imagCoeff.toDouble
        val imagPart = if (sign == "-") -coefficient else coefficient
        Some(ComplexDouble(realPart, imagPart))

      case _ => None
    }
  }

  def fromInt(x: Int): ComplexDouble = x

  def toInt(x: ComplexDouble): Int = x.intValue

  def toLong(x: ComplexDouble): Long = x.longValue

  def toFloat(x: ComplexDouble): Float = x.floatValue

  def toDouble(x: ComplexDouble): Double = x.doubleValue

  def compare(x: ComplexDouble, y: ComplexDouble): Int = sys.error("can't compare complex numbers")

}

object ComplexDoubleIsFractional {

  // Pure imaginary: "4i", "-4i", "i", "-i"
  private val PURE_IMAGINARY = """^(-?)(\d*(?:\.\d*)?(?:[eE][+-]?\d+)?)i$""".r

  // Full complex: "3+4i", "3-4i", "3+i", "3-i"
  private val FULL_COMPLEX = """^(-?\d+(?:\.\d*)?(?:[eE][+-]?\d+)?)([+-])(\d*(?:\.\d*)?(?:[eE][+-]?\d+)?)i$""".r

  implicit object complexDoubleIsFractional extends ComplexDoubleIsFractional

}
