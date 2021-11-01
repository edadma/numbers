package io.github.edadma.numbers

case class QuaternionBigInt(a: BigInt, b: BigInt, c: BigInt, d: BigInt) extends AbstractQuaternionRational[BigInt, QuaternionBigInt] {

  protected def fractional(a: BigInt): Double = a.doubleValue

  protected def quaternion(a: BigInt, b: BigInt, c: BigInt, d: BigInt): QuaternionBigInt = QuaternionBigInt(a, b, c, d)

  protected def promote: QuaternionDouble = QuaternionDouble(a.doubleValue, b.doubleValue, c.doubleValue, d.doubleValue)

  protected def divide(a: BigInt, b: BigInt): BigInt = unsup

  def zero: QuaternionBigInt = QuaternionBigInt.zero

  def one: QuaternionBigInt = QuaternionBigInt.one

  def i: QuaternionBigInt = QuaternionBigInt.i

  def doubleValue: Double = abs

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

}

object QuaternionBigInt {

  val i: QuaternionBigInt = QuaternionBigInt(0, 1, 0, 0)
  val j: QuaternionBigInt = QuaternionBigInt(0, 0, 1, 0)
  val k: QuaternionBigInt = QuaternionBigInt(0, 0, 0, 1)
  val zero: QuaternionBigInt = QuaternionBigInt(0, 0, 0, 0)
  val one: QuaternionBigInt = QuaternionBigInt(1, 0, 0, 0)

  def apply(a: BigInt) = new QuaternionBigInt(a, 0, 0, 0)

  implicit def int2complex(a: Int): QuaternionBigInt = QuaternionBigInt(a)

  implicit def bigint2complex(a: BigInt): QuaternionBigInt = QuaternionBigInt(a)

}
