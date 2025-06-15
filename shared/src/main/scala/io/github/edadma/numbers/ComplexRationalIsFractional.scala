package io.github.edadma.numbers

trait ComplexRationalIsFractional extends Fractional[ComplexRational] {

  def plus(x: ComplexRational, y: ComplexRational): ComplexRational = x + y

  def minus(x: ComplexRational, y: ComplexRational): ComplexRational = x - y

  def times(x: ComplexRational, y: ComplexRational): ComplexRational = x * y

  def div(x: ComplexRational, y: ComplexRational): ComplexRational = x / y

  def negate(x: ComplexRational): ComplexRational = -x

  def parseString(str: String): Option[ComplexRational] = {
    import ComplexRationalIsFractional.{COMPLEX_RATIONAL, PURE_RATIONAL_IMAGINARY}

    str.trim match {
      // Try pure rational imaginary first (e.g., "2/3i", "-2/3i")
      case PURE_RATIONAL_IMAGINARY(im) =>
        try {
          val imagPart = Rational(im)
          Some(ComplexRational(Rational(0), imagPart))
        } catch {
          case _: IllegalArgumentException => None
          case _: NumberFormatException    => None
        }
      // Then try full complex rational (e.g., "1/2+1/3i", "1/2-1/3i")
      case COMPLEX_RATIONAL(re, sign, im) =>
        try {
          val realPart = Rational(re)
          val imagPart = if (im == null || im.isEmpty) {
            if (sign == "-") Rational(-1) else Rational(1)
          } else {
            val imagRational = Rational(im)
            if (sign == "-") -imagRational else imagRational
          }
          Some(ComplexRational(realPart, imagPart))
        } catch {
          case _: IllegalArgumentException => None // Handle division by zero, etc.
          case _: NumberFormatException    => None // Handle malformed numbers
        }
      case _ => None
    }
  }

  def fromInt(x: Int): ComplexRational = x

  def toInt(x: ComplexRational): Int = x.intValue

  def toLong(x: ComplexRational): Long = x.longValue

  def toFloat(x: ComplexRational): Float = x.floatValue

  def toDouble(x: ComplexRational): Double = x.doubleValue

  def compare(x: ComplexRational, y: ComplexRational): Int = sys.error("can't compare complex numbers")

}

object ComplexRationalIsFractional {

  private val COMPLEX_RATIONAL        = """(-?\d+/\d+)([+-])(\d+/\d+)?i""".r
  private val PURE_RATIONAL_IMAGINARY = """(-?\d+/\d+)i""".r

  implicit object complexRationalIsFractional extends ComplexRationalIsFractional

}
