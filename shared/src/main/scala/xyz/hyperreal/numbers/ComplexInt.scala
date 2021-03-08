package xyz.hyperreal.numbers

case class ComplexInt(re: Int, im: Int) extends AbstractComplexRational[Int, ComplexInt] {

  protected def fractional(a: Int): Double = a.toDouble

  protected def complex(re: Int, im: Int): ComplexInt = ComplexInt(re, im)

  protected def promote: ComplexDouble = ComplexDouble(re, im)

  protected def divide(a: Int, b: Int): Int = unsup

  def zero: ComplexInt = ComplexInt.zero

  def one: ComplexInt = ComplexInt.one

  def i: ComplexInt = ComplexInt.i

  def doubleValue: Double = abs

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

}

object ComplexInt {

  val i: ComplexInt = ComplexInt(0, 1)

  val zero: ComplexInt = ComplexInt(0, 0)

  val one: ComplexInt = ComplexInt(1, 0)

  def apply(a: Int) = new ComplexInt(a, 0)

  implicit def int2complex(a: Int): ComplexInt = ComplexInt(a)

}
