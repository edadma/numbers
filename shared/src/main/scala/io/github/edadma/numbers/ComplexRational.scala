package io.github.edadma.numbers

case class ComplexRational(re: Rational, im: Rational) extends AbstractComplexRational[Rational, ComplexRational] {

  protected def fractional(a: Rational): Double = a.doubleValue

  protected def complex(re: Rational, im: Rational): ComplexRational = ComplexRational(re, im)

  protected def promote: ComplexDouble = ComplexDouble(re.doubleValue, im.doubleValue)

  protected def divide(a: Rational, b: Rational): Rational = a / b

  def zero: ComplexRational = ComplexRational.zero

  def one: ComplexRational = ComplexRational.one

  def i: ComplexRational = ComplexRational.i

  def doubleValue: Double = abs.doubleValue

  def floatValue: Float = abs.floatValue

  def intValue: Int = abs.intValue

  def longValue: Long = abs.longValue

}

object ComplexRational {

  val i: ComplexRational = ComplexRational(0, 1)

  val zero: ComplexRational = ComplexRational(0, 0)

  val one: ComplexRational = ComplexRational(1, 0)

  def apply(a: Rational) = new ComplexRational(a, 0)

  implicit def int2complex(a: Int): ComplexRational = ComplexRational(a)

  implicit def rational2complex(a: Rational): ComplexRational = ComplexRational(a)

}
