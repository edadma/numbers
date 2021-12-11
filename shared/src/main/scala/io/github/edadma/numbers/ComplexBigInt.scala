package io.github.edadma.numbers

class ComplexBigInt(val re: BigInt, val im: BigInt) extends AbstractComplexRational[BigInt, ComplexBigInt] {

  protected def fractional(a: BigInt): Double = a.doubleValue

  protected def complex(re: BigInt, im: BigInt): ComplexBigInt = ComplexBigInt(re, im)

  protected def promote: ComplexDouble = ComplexDouble(re.doubleValue, im.doubleValue)

  protected def divide(a: BigInt, b: BigInt): BigInt = unsup

  def zero: ComplexBigInt = ComplexBigInt.zero

  def one: ComplexBigInt = ComplexBigInt.one

  def i: ComplexBigInt = ComplexBigInt.i

  def doubleValue: Double = abs

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

  override def equals(o: Any): Boolean =
    o match {
      case r: Int           => re == 0 && re == r
      case r: BigInt        => re == 0 && re == r
      case z: ComplexBigInt => re == z.re && im == z.im
      case _                => false
    }

}

object ComplexBigInt {

  def apply(re: BigInt, im: BigInt) = new ComplexBigInt(re, im)

  val i: ComplexBigInt = ComplexBigInt(0, 1)

  val zero: ComplexBigInt = ComplexBigInt(0, 0)

  val one: ComplexBigInt = ComplexBigInt(1, 0)

  def apply(a: BigInt) = new ComplexBigInt(a, 0)

  implicit def int2complex(a: Int): ComplexBigInt = ComplexBigInt(a)

  implicit def bigint2complex(a: BigInt): ComplexBigInt = ComplexBigInt(a)

}
