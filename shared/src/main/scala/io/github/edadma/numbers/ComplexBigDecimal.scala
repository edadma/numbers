package io.github.edadma.numbers

case class ComplexBigDecimal(re: BigDecimal, im: BigDecimal)(implicit val bdmath: BigDecimalMath)
    extends Complex[BigDecimal, BigDecimal, ComplexBigDecimal, ComplexBigDecimal] {

  protected def promote(re: BigDecimal, im: BigDecimal): ComplexBigDecimal =
    ComplexBigDecimal(re, im)

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

  protected def _pow(a: BigDecimal, b: BigDecimal): BigDecimal = BigDecimalMath.pow(a, b)

  protected def fractional(a: BigDecimal): BigDecimal = a

  protected def complex(re: BigDecimal, im: BigDecimal): ComplexBigDecimal =
    ComplexBigDecimal(re, im)

  protected def promote: ComplexBigDecimal = this

  protected def divide(a: BigDecimal, b: BigDecimal): BigDecimal = a / b

  def ^(p: Int): ComplexBigDecimal = ^(complex(p))

  def ^(p: BigInt): ComplexBigDecimal = ^(complex(BigDecimal(p)))

  def zero: ComplexBigDecimal = bdmath.ZEROC.v

  def one: ComplexBigDecimal = bdmath.ONEC.v: ComplexBigDecimal

  def i: ComplexBigDecimal = bdmath.IC.v

  def doubleValue: Double = abs.toDouble

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

}

object ComplexBigDecimal {

  def i(implicit bdmath: BigDecimalMath): ComplexBigDecimal = ComplexBigDecimal(0, 1)

  def apply(a: BigDecimal)(implicit bdmath: BigDecimalMath) =
    new ComplexBigDecimal(a, 0)

  implicit def int2complex(a: Int)(implicit bdmath: BigDecimalMath): ComplexBigDecimal =
    ComplexBigDecimal(a)

  implicit def bigDecimal2complex(a: BigDecimal)(implicit bdmath: BigDecimalMath): ComplexBigDecimal = ComplexBigDecimal(a)
}
