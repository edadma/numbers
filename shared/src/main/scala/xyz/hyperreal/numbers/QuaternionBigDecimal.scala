package xyz.hyperreal.numbers

case class QuaternionBigDecimal(a: BigDecimal, b: BigDecimal, c: BigDecimal, d: BigDecimal)(implicit val bdmath: BigDecimalMath)
    extends Quaternion[BigDecimal, BigDecimal, QuaternionBigDecimal, QuaternionBigDecimal] {

  protected def promote(a: BigDecimal, b: BigDecimal, c: BigDecimal, d: BigDecimal): QuaternionBigDecimal =
    QuaternionBigDecimal(a, b, c, d)

  protected def _floor(a: BigDecimal): BigDecimal =
    a.setScale(0, BigDecimal.RoundingMode.FLOOR)

  protected def _ceil(a: BigDecimal): BigDecimal =
    a.setScale(0, BigDecimal.RoundingMode.CEILING)

  protected def _sqrt(a: BigDecimal): BigDecimal = BigDecimalMath.sqrt(a)

  protected def _atan2(y: BigDecimal, x: BigDecimal): BigDecimal =
    BigDecimalMath.atan2(y, x)

  protected def _ln(a: BigDecimal): BigDecimal = BigDecimalMath.ln(a)

  protected def _exp(a: BigDecimal): BigDecimal = BigDecimalMath.exp(a)

  protected def _sin(a: BigDecimal): BigDecimal = BigDecimalMath.sin(a)

  protected def _cos(a: BigDecimal): BigDecimal = BigDecimalMath.cos(a)

  protected def _acos(a: BigDecimal): BigDecimal = BigDecimalMath.acos(a)

  protected def _pow(a: BigDecimal, b: BigDecimal): BigDecimal = BigDecimalMath.pow(a, b)

  protected def fdivide(a: BigDecimal, b: BigDecimal): BigDecimal = a / b

  protected def fmul(a: BigDecimal, b: BigDecimal): BigDecimal = a * b

  protected def fractional(a: BigDecimal): BigDecimal = a

  protected def quaternion(a: BigDecimal, b: BigDecimal, c: BigDecimal, d: BigDecimal): QuaternionBigDecimal =
    QuaternionBigDecimal(a, b, c, d)

  protected def promote: QuaternionBigDecimal = this

  protected def divide(a: BigDecimal, b: BigDecimal): BigDecimal = a / b

  def ^(p: Int): QuaternionBigDecimal = ^(quaternion(p))

  def ^(p: BigInt): QuaternionBigDecimal = ^(quaternion(BigDecimal(p)))

  def zero: QuaternionBigDecimal = bdmath.ZEROQ.v

  def one: QuaternionBigDecimal = bdmath.ONEQ.v

  def i: QuaternionBigDecimal = bdmath.IQ.v

  def j: QuaternionBigDecimal = bdmath.JQ.v

  def k: QuaternionBigDecimal = bdmath.KQ.v

  def doubleValue: Double = abs.toDouble

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

}

object QuaternionBigDecimal {

  def i(implicit bdmath: BigDecimalMath): QuaternionBigDecimal = bdmath.IQ.v

  def j(implicit bdmath: BigDecimalMath): QuaternionBigDecimal = bdmath.JQ.v

  def k(implicit bdmath: BigDecimalMath): QuaternionBigDecimal = bdmath.KQ.v

  def apply(a: BigDecimal)(implicit bdmath: BigDecimalMath) =
    new QuaternionBigDecimal(a, 0, 0, 0)

  implicit def int2quaternion(a: Int)(implicit bdmath: BigDecimalMath): QuaternionBigDecimal =
    QuaternionBigDecimal(a)

  implicit def bigDecimal2quaternion(a: BigDecimal)(implicit bdmath: BigDecimalMath): QuaternionBigDecimal = QuaternionBigDecimal(a)
}
