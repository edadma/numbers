package io.github.edadma.numbers

import java.math.MathContext
import math.*
import scala.language.implicitConversions

class Rational(n: BigInt, d: BigInt) extends Number with Ordered[Rational] {
  import Rational._

  require(d != ZERObi, "denominator can't be zero")

  val (numerator, denominator) = {
    val g    = n gcd d
    val sign = d.signum

    (n / g * sign, d / g * sign)
  }

  def isZero: Boolean = numerator == ZERObi

  def nonZero: Boolean = numerator != ZERObi

  def maybeDemote: Number =
    if (isWhole)
      if (numerator.isValidInt)
        Integer.valueOf(numerator.toInt)
      else
        numerator
    else
      this

  def signum: Int = numerator.signum

  def isPositive: Boolean = numerator > ZERObi

  def isNegative: Boolean = numerator < ZERObi

  def isWhole: Boolean = denominator == ONEbi

  def +(that: Rational): Rational =
    Rational(numerator * that.denominator + that.numerator * denominator, denominator * that.denominator)

  def +(that: BigInt): Rational = Rational(numerator + that * denominator, denominator)

  def *(that: Rational): Rational = Rational(numerator * that.numerator, denominator * that.denominator)

  def *(that: BigInt): Rational = Rational(numerator * that, denominator)

  def -(that: Rational): Rational =
    Rational(numerator * that.denominator - that.numerator * denominator, denominator * that.denominator)

  def -(that: BigInt): Rational = Rational(numerator - that * denominator, denominator)

  def /(that: Rational): Rational =
    if (that.isZero) sys.error("division by zero")
    Rational(numerator * that.denominator, denominator * that.numerator)

  def /(that: BigInt): Rational = Rational(numerator, denominator * that)

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

  def unary_- : Rational = Rational(-numerator, denominator)

  def inv: Rational =
    if (isZero)
      sys.error("no inverse")
    else
      Rational(denominator, numerator)

  def abs: Rational = if (numerator > ZERObi) this else new Rational(numerator.abs, denominator.abs)

  def floor: Rational = {
    val rem = numerator.abs mod denominator

    Rational(if (numerator > ZERObi) numerator - rem else numerator - denominator + rem, denominator)
  }

  def ceil: Rational = {
    val rem = numerator.abs mod denominator

    Rational(if (numerator < ZERObi) numerator + rem else numerator + denominator - rem, denominator)
  }

  def toMixedNumber: (BigInt, Rational) = (numerator / denominator, Rational(numerator % denominator, denominator))

  def mediant(that: Rational): Rational = Rational(numerator + that.numerator, denominator + that.denominator)

  def decimalValue(mc: MathContext): BigDecimal = BigDecimal(numerator, mc) / BigDecimal(denominator, mc)

  def doubleValue: Double =
    if (numerator.isValidDouble && denominator.isValidDouble)
      numerator.toDouble / denominator.toDouble
    else
      (BigDecimal(numerator) / BigDecimal(denominator)).toDouble

  def floatValue: Float =
    if (numerator.isValidFloat && denominator.isValidFloat)
      numerator.toFloat / denominator.toFloat
    else
      (BigDecimal(numerator) / BigDecimal(denominator)).toFloat

  def intValue: Int = (numerator / denominator).toInt

  def longValue: Long = (numerator / denominator).toLong

  def compare(that: Rational): Int = (numerator * that.denominator).compare(that.numerator * denominator)

  override def equals(x: Any): Boolean =
    x match {
      case r: Rational => numerator == r.numerator && denominator == r.denominator
      case bi: BigInt  => isWhole && numerator == bi
      case l: Long     => isWhole && numerator == l
      case i: Int      => isWhole && numerator == i
      case _           => false
    }

  override def toString: String =
    if (isZero) "0" else if (isWhole) numerator.toString else s"$numerator/$denominator"
}

object Rational {
  private val ZERObi   = BigInt(0)
  private val ONEbi    = BigInt(1)
  private val RATIONAL = """\s*(-?\d+)\s*/\s*(\d+)\s*""".r

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
      case r: Rational => Some(r.numerator, r.denominator)
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

  def parseString(s: String): Option[Rational] =
    try Some(Rational(s))
    catch { case _: Exception => None }

  def fromInt(x: Int): Rational = x

  def toInt(x: Rational): Int = x.intValue

  def toLong(x: Rational): Long = x.longValue

  def toFloat(x: Rational): Float = x.floatValue

  def toDouble(x: Rational): Double = x.doubleValue

  def compare(x: Rational, y: Rational): Int = x.compare(y)

}
