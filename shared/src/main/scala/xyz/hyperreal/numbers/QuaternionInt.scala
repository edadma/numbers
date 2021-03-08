package xyz.hyperreal.numbers

case class QuaternionInt(a: Int, b: Int, c: Int, d: Int) extends AbstractQuaternionRational[Int, QuaternionInt] {

  protected def fractional(a: Int): Double = a.toDouble

  protected def quaternion(a: Int, b: Int, c: Int, d: Int): QuaternionInt = QuaternionInt(a, b, c, d)

  protected def promote: QuaternionDouble = QuaternionDouble(a, b, c, d)

  protected def divide(a: Int, b: Int): Int = unsup

  def zero: QuaternionInt = QuaternionInt.zero

  def one: QuaternionInt = QuaternionInt.one

  def i: QuaternionInt = QuaternionInt.i

  def doubleValue: Double = abs

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

}

object QuaternionInt {

  val i: QuaternionInt = QuaternionInt(0, 1, 0, 0)

  val j: QuaternionInt = QuaternionInt(0, 0, 1, 0)

  val k: QuaternionInt = QuaternionInt(0, 0, 0, 1)

  val zero: QuaternionInt = QuaternionInt(0, 0, 0, 0)

  val one: QuaternionInt = QuaternionInt(1, 0, 0, 0)

  def apply(a: Int) = new QuaternionInt(a, 0, 0, 0)

  implicit def int2quaternion(a: Int): QuaternionInt = QuaternionInt(a)

}
