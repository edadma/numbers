package io.github.edadma.numbers

import java.math.MathContext

import math._

class Rational(_n: BigInt, _d: BigInt) extends Number with Ordered[Rational] {
  import Rational._

  require(_d != ZERObi, "denominator can't be zero")

  val (n, d) = {
    val g    = _n gcd _d
    val sign = _d.signum

    (_n / g * sign, _d / g * sign)
  }

  def isZero: Boolean = n == ZERObi

  def isInt: Boolean = d == ONEbi

  def maybeDemote: Number =
    if (isInt)
      if (n.isValidInt)
        Integer.valueOf(n.toInt)
      else
        n
    else
      this

  def +(that: Rational): Rational = Rational(n * that.d + that.n * d, d * that.d)

  def +(that: BigInt): Rational = Rational(n + that * d, d)

  def *(that: Rational): Rational = Rational(n * that.n, d * that.d)

  def *(that: BigInt): Rational = Rational(n * that, d)

  def -(that: Rational): Rational = Rational(n * that.d - that.n * d, d * that.d)

  def -(that: BigInt): Rational = Rational(n - that * d, d)

  def /(that: Rational): Rational = Rational(n * that.d, d * that.n)

  def /(that: BigInt): Rational = Rational(n, d * that)

  def ^(that: Int): Rational = {
    def _pow(b: Rational, e: Int): Rational =
      if (e == 1)
        b
      else if ((e & 1) == 1)
        b * _pow(b * b, (e - 1) / 2)
      else
        _pow(b * b, e / 2)

    if (isZero)
      if (that == 0)
        sys.error("0^0 is undefined")
      else
        ZERO
    else if (that == 0)
      ONE
    else if (that < 0)
      _pow(this.inv, -that)
    else
      _pow(this, that)
  }

  def ^(that: BigInt): Rational = {
    def _pow(b: Rational, e: BigInt): Rational =
      if (e == 1)
        b
      else if (e testBit 0)
        b * _pow(b * b, (e - 1) / 2)
      else
        _pow(b * b, e / 2)

    if (isZero)
      if (that == ZERObi)
        sys.error("0^0 is undefined")
      else
        ZERO
    else if (that == ZERObi)
      ONE
    else if (that < ZERObi)
      _pow(this.inv, -that)
    else
      _pow(this, that)
  }

  def unary_- : Rational = Rational(-n, d)

  def inv: Rational =
    if (isZero)
      sys.error("no inverse")
    else
      Rational(d, n)

  def abs: Rational = if (n > ZERObi) this else new Rational(n.abs, d.abs)

  def floor: Rational = {
    val rem = n.abs mod d

    Rational(if (n > ZERObi) n - rem else n - d + rem, d)
  }

  def ceil: Rational = {
    val rem = n.abs mod d

    Rational(if (n < ZERObi) n + rem else n + d - rem, d)
  }

  def decimalValue(mc: MathContext): BigDecimal = BigDecimal(n, mc) / BigDecimal(d, mc)

  def doubleValue: Double =
    if (n.isValidDouble && d.isValidDouble)
      n.toDouble / d.toDouble
    else
      (BigDecimal(n) / BigDecimal(d)).toDouble

  def floatValue: Float =
    if (n.isValidFloat && d.isValidFloat)
      n.toFloat / d.toFloat
    else
      (BigDecimal(n) / BigDecimal(d)).toFloat

  def intValue: Int = (n / d).toInt

  def longValue: Long = (n / d).toLong

  def compare(that: Rational): Int = (n * that.d).compare(that.n * d)

  override def equals(x: Any): Boolean =
    x match {
      case r: Rational => n == r.n && d == r.d
      case bi: BigInt  => isInt && n == bi
      case l: Long     => isInt && n == l
      case i: Int      => isInt && n == i
      case _           => false
    }

  override def toString: String =
    if (isZero) "0" else if (isInt) n.toString else s"$n/$d"
}

object Rational {
  private val ZERObi   = BigInt(0)
  private val ONEbi    = BigInt(1)
  private val RATIONAL = """(-?\d+)/(\d+)""" r

  lazy val ZERO: Rational = Rational(0)
  lazy val ONE: Rational  = Rational(1)

  def oneOver(d: BigInt) = new Rational(ONEbi, d)

  def apply(s: String): Rational =
    s match {
      case RATIONAL(n, d) => new Rational(BigInt(n), BigInt(d))
      case _              => sys.error("not a rational")
    }

  def apply(n: BigInt, d: BigInt) = new Rational(n, d)

  def apply(a: BigInt) = new Rational(a, ONEbi)

  def apply(a: Int) = new Rational(BigInt(a), ONEbi)

  def apply(a: Long) = new Rational(BigInt(a), ONEbi)

  def unapply(z: Any): Option[(BigInt, BigInt)] =
    z match {
      case r: Rational => Some(r.n, r.d)
      case _           => None
    }

  implicit def int2rational(a: Int): Rational = Rational(a)

  implicit def bigint2rational(a: BigInt): Rational = Rational(a)

  implicit def intdiv2rational(a: Int): IntDiv = new IntDiv(a)

  class IntDiv(a: Int) {
    def \(b: Int) = new Rational(a, b)
  }

  implicit object rationalIsFractional extends RationalIsFractional
}

trait RationalIsFractional extends Fractional[Rational] {

  def plus(x: Rational, y: Rational): Rational = x + y

  def minus(x: Rational, y: Rational): Rational = x - y

  def times(x: Rational, y: Rational): Rational = x * y: Rational

  def div(x: Rational, y: Rational): Rational = x / y

  def negate(x: Rational): Rational = -x

  def parseString(s: String): Option[Rational] = Some(Rational(s))

  def fromInt(x: Int): Rational = x

  def toInt(x: Rational): Int = x.intValue

  def toLong(x: Rational): Long = x.longValue

  def toFloat(x: Rational): Float = x.floatValue

  def toDouble(x: Rational): Double = x.doubleValue

  def compare(x: Rational, y: Rational): Int = x.compare(y)

}
