package xyz.hyperreal.numbers

trait ComplexDoubleIsFractional extends Fractional[ComplexDouble] {

  def plus(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x + y

  def minus(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x - y

  def times(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x * y

  def div(x: ComplexDouble, y: ComplexDouble): ComplexDouble = x / y

  def negate(x: ComplexDouble): ComplexDouble = -x

  def parseString(str: String): Option[ComplexDouble] = {
    import ComplexDoubleIsFractional.COMPLEX_DOUBLE

    str match {
      case COMPLEX_DOUBLE(re, im) => Some(ComplexDouble(re.toDouble, im.toDouble))
      case _                      => None
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

  private val COMPLEX_DOUBLE = """(-?(?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)[+-]((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?i""".r

  implicit object complexDoubleIsFractional extends ComplexDoubleIsFractional

}
