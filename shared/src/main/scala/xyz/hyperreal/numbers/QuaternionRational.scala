package xyz.hyperreal.numbers

case class QuaternionRational(a: Rational, b: Rational, c: Rational, d: Rational) extends AbstractQuaternionRational[Rational, QuaternionRational] {

  protected def fractional(a: Rational): Double = a.doubleValue

  protected def quaternion(a: Rational, b: Rational, c: Rational, d: Rational): QuaternionRational = QuaternionRational(a, b, c, d)

  protected def promote: QuaternionDouble = QuaternionDouble(a.doubleValue, b.doubleValue, c.doubleValue, d.doubleValue)

  protected def divide(a: Rational, b: Rational): Rational = a / b

  def zero: QuaternionRational = QuaternionRational.zero

  def one: QuaternionRational = QuaternionRational.one

  def i: QuaternionRational = QuaternionRational.i

  def doubleValue: Double = abs.doubleValue

  def floatValue: Float = abs.floatValue

  def intValue: Int = abs.intValue

  def longValue: Long = abs.longValue

}

object QuaternionRational {

  val i: QuaternionRational = QuaternionRational(0, 1, 0, 0)

  val j: QuaternionRational = QuaternionRational(0, 0, 1, 0)

  val k: QuaternionRational = QuaternionRational(0, 0, 0, 1)

  val zero: QuaternionRational = QuaternionRational(0, 0, 0, 0)

  val one: QuaternionRational = QuaternionRational(1, 0, 0, 0)

  def apply(a: Rational) = new QuaternionRational(a, 0, 0, 0)

  implicit def int2quaternion(a: Int): QuaternionRational = QuaternionRational(a)

  implicit def rational2quaternion(a: Rational): QuaternionRational = QuaternionRational(a)

}
