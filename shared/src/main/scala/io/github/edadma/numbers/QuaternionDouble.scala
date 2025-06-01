package io.github.edadma.numbers

import math._

case class QuaternionDouble(a: Double, b: Double, c: Double, d: Double)
    extends AbstractQuaternionRational[Double, QuaternionDouble] {

  protected def fractional(a: Double): Double = a

  protected def quaternion(a: Double, b: Double, c: Double, d: Double): QuaternionDouble = QuaternionDouble(a, b, c, d)

  protected def promote: QuaternionDouble = this

  protected def divide(a: Double, b: Double): Double = a / b

  def zero: QuaternionDouble = QuaternionDouble.zero

  def one: QuaternionDouble = QuaternionDouble.one

  def i: QuaternionDouble = QuaternionDouble.i

  def doubleValue: Double = abs

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

  def roughly(x: Double, y: Double): Boolean = (x - y).abs < 1e-13

  def roughly(that: QuaternionDouble): Boolean =
    roughly(a, that.a) && roughly(b, that.b) && roughly(c, that.c) && roughly(d, that.d)

  def nearly(x: Double, y: Double): Boolean = (x - y).abs < 1e-15

  override def equals(o: Any): Boolean =
    o match {
      case r: QuaternionDouble => nearly(a, r.a) && nearly(b, r.b) && nearly(c, r.c) && nearly(d, r.d)
      case r: Int              => nearly(b, 0) && nearly(c, 0) && nearly(d, 0) && nearly(a, r)
      case r: Double           => nearly(b, 0) && nearly(c, 0) && nearly(d, 0) && nearly(a, r)
      case _: BigInt | _: BigDecimal | _: Rational =>
        nearly(b, 0) && nearly(c, 0) && nearly(d, 0) && nearly(a, o.asInstanceOf[Number].doubleValue)
      case _ => false
    }

  override def toString: String = {
    def formatDouble(d: Double): String = {
      if (d == d.toInt.toDouble) d.toInt.toString
      else d.toString
    }

    if (this == zero)
      "0"
    else {
      val buf = new StringBuilder

      def imag(v: Double, basis: Char): Unit =
        if (v != 0.0) {
          if (v == 1.0) {
            if (buf.nonEmpty)
              buf += '+'
            buf += basis
          } else if (v == -1.0)
            buf ++= s"-$basis"
          else if (v < 0.0)
            buf ++= s"${formatDouble(v)}$basis"
          else {
            if (buf.nonEmpty)
              buf += '+'
            buf ++= s"${formatDouble(v)}$basis"
          }
        }

      if (a != 0.0)
        buf ++= formatDouble(a)

      imag(b, 'i')
      imag(c, 'j')
      imag(d, 'k')
      buf.toString
    }
  }

}

object QuaternionDouble {
  val i: QuaternionDouble = QuaternionDouble(0, 1, 0, 0)

  val j: QuaternionDouble = QuaternionDouble(0, 0, 1, 0)

  val k: QuaternionDouble = QuaternionDouble(0, 0, 0, 1)

  val zero: QuaternionDouble = QuaternionDouble(0, 0, 0, 0)

  val one: QuaternionDouble = QuaternionDouble(1, 0, 0, 0)

  def apply(a: Double) = new QuaternionDouble(a, 0, 0, 0)

  implicit def int2quaternion(a: Int): QuaternionDouble = QuaternionDouble(a)

  implicit def double2quaternion(a: Double): QuaternionDouble = QuaternionDouble(a)
}
