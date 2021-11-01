package io.github.edadma.numbers

trait QuaternionDoubleIsFractional extends Fractional[QuaternionDouble] {

  def plus(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x + y

  def minus(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x - y

  def times(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x * y

  def div(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x / y

  def negate(x: QuaternionDouble): QuaternionDouble = -x

  def parseString(str: String): Option[QuaternionDouble] = {
    import QuaternionDoubleIsFractional.QUATERNION_DOUBLE

    str match {
      case QUATERNION_DOUBLE(a, b, c, d) => Some(QuaternionDouble(a.toDouble, b.toDouble, c.toDouble, d.toDouble))
      case _                             => None
    }
  }

  def fromInt(x: Int): QuaternionDouble = x

  def toInt(x: QuaternionDouble): Int = x.intValue

  def toLong(x: QuaternionDouble): Long = x.longValue

  def toFloat(x: QuaternionDouble): Float = x.floatValue

  def toDouble(x: QuaternionDouble): Double = x.doubleValue

  def compare(x: QuaternionDouble, y: QuaternionDouble): Int = sys.error("can't compare quaternion numbers")

}

object QuaternionDoubleIsFractional {

  private val QUATERNION_DOUBLE =
    """(-?(?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)[+-]((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?i[+-]((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?j[+-]((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?k""".r

  implicit object quaternionDoubleIsFractional extends QuaternionDoubleIsFractional

}
