package io.github.edadma.numbers

trait ComplexRationalIsFractional extends Fractional[ComplexRational] {

  def plus(x: ComplexRational, y: ComplexRational): ComplexRational = x + y

  def minus(x: ComplexRational, y: ComplexRational): ComplexRational = x - y

  def times(x: ComplexRational, y: ComplexRational): ComplexRational = x * y

  def div(x: ComplexRational, y: ComplexRational): ComplexRational = x / y

  def negate(x: ComplexRational): ComplexRational = -x

  def parseString(str: String): Option[ComplexRational] = {
    import ComplexRationalIsFractional.COMPLEX_RATIONAL

    str match {
      case COMPLEX_RATIONAL(re, im) => Some(ComplexRational(Rational(re), Rational(im)))
      case _                        => None
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

  private val COMPLEX_RATIONAL = """(-?\d+/\d+)[+-](\d+/\d+)?i""".r

  implicit object complexRationalIsFractional extends ComplexRationalIsFractional

}
