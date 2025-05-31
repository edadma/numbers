package io.github.edadma.numbers

import java.math.MathContext
import math.*
import scala.language.implicitConversions

class SmallRational(n: Long, d: Long) extends Number with Ordered[SmallRational] {
  import SmallRational._

  require(d != 0L, "denominator can't be zero")

  val (numerator, denominator) = {
    val g    = gcd(math.abs(n), math.abs(d))
    val sign = if (d < 0) -1 else 1
    (n / g * sign, d / g * sign)
  }

  def isZero: Boolean = numerator == 0L

  def nonZero: Boolean = numerator != 0L

  def maybeDemote: Number =
    if (isWhole)
      if (numerator >= Int.MinValue && numerator <= Int.MaxValue)
        Integer.valueOf(numerator.toInt)
      else
        java.lang.Long.valueOf(numerator)
    else
      this

  def signum: Int = java.lang.Long.signum(numerator)

  def isPositive: Boolean = numerator > 0L

  def isNegative: Boolean = numerator < 0L

  def isWhole: Boolean = denominator == 1L

  def +(that: SmallRational): SmallRational = {
    val newNum = numerator * that.denominator + that.numerator * denominator
    val newDen = denominator * that.denominator
    SmallRational(newNum, newDen)
  }

  def +(that: Long): SmallRational = {
    SmallRational(numerator + that * denominator, denominator)
  }

  def +(that: Int): SmallRational = this + that.toLong

  def *(that: SmallRational): SmallRational = {
    SmallRational(numerator * that.numerator, denominator * that.denominator)
  }

  def *(that: Long): SmallRational = {
    SmallRational(numerator * that, denominator)
  }

  def *(that: Int): SmallRational = this * that.toLong

  def -(that: SmallRational): SmallRational = {
    val newNum = numerator * that.denominator - that.numerator * denominator
    val newDen = denominator * that.denominator
    SmallRational(newNum, newDen)
  }

  def -(that: Long): SmallRational = {
    SmallRational(numerator - that * denominator, denominator)
  }

  def -(that: Int): SmallRational = this - that.toLong

  def /(that: SmallRational): SmallRational = {
    if (that.isZero) sys.error("division by zero")
    SmallRational(numerator * that.denominator, denominator * that.numerator)
  }

  def /(that: Long): SmallRational = {
    if (that == 0L) sys.error("division by zero")
    SmallRational(numerator, denominator * that)
  }

  def /(that: Int): SmallRational = this / that.toLong

  // Cross-platform overflow-checking variants for when you need them
  def addExact(that: SmallRational): SmallRational = {
    val newNum = SmallRational.checkAddOverflow(
      SmallRational.checkMultiplyOverflow(numerator, that.denominator),
      SmallRational.checkMultiplyOverflow(that.numerator, denominator),
    )
    val newDen = SmallRational.checkMultiplyOverflow(denominator, that.denominator)
    SmallRational(newNum, newDen)
  }

  def multiplyExact(that: SmallRational): SmallRational = {
    val newNum = SmallRational.checkMultiplyOverflow(numerator, that.numerator)
    val newDen = SmallRational.checkMultiplyOverflow(denominator, that.denominator)
    SmallRational(newNum, newDen)
  }

  def subtractExact(that: SmallRational): SmallRational = {
    val newNum = SmallRational.checkSubtractOverflow(
      SmallRational.checkMultiplyOverflow(numerator, that.denominator),
      SmallRational.checkMultiplyOverflow(that.numerator, denominator),
    )
    val newDen = SmallRational.checkMultiplyOverflow(denominator, that.denominator)
    SmallRational(newNum, newDen)
  }

  def divideExact(that: SmallRational): SmallRational = {
    if (that.isZero) sys.error("division by zero")
    val newNum = SmallRational.checkMultiplyOverflow(numerator, that.denominator)
    val newDen = SmallRational.checkMultiplyOverflow(denominator, that.numerator)
    SmallRational(newNum, newDen)
  }

  def ^(that: Int): SmallRational = {
    def _pow(b: SmallRational, e: Int): SmallRational =
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

  def unary_- : SmallRational = SmallRational(-numerator, denominator)

  def inv: SmallRational =
    if (isZero)
      sys.error("no inverse")
    else
      SmallRational(denominator, numerator)

  def abs: SmallRational =
    if (numerator >= 0) this
    else SmallRational(-numerator, denominator)

  def floor: SmallRational = {
    if (isWhole) {
      this
    } else {
      val quotient = numerator / denominator
      if (numerator < 0) {
        SmallRational(quotient - 1L, 1L)
      } else {
        SmallRational(quotient, 1L)
      }
    }
  }

  def ceil: SmallRational = {
    if (isWhole) {
      this
    } else {
      val quotient = numerator / denominator
      if (numerator > 0) {
        SmallRational(quotient + 1L, 1L)
      } else {
        SmallRational(quotient, 1L)
      }
    }
  }

  def toMixedNumber: (Long, SmallRational) =
    (numerator / denominator, SmallRational(numerator % denominator, denominator))

  def mediant(that: SmallRational): SmallRational = {
    SmallRational(numerator + that.numerator, denominator + that.denominator)
  }

  def decimalValue(mc: MathContext): BigDecimal =
    BigDecimal(numerator, mc) / BigDecimal(denominator, mc)

  def doubleValue: Double = numerator.toDouble / denominator.toDouble

  def floatValue: Float = numerator.toFloat / denominator.toFloat

  def intValue: Int = (numerator / denominator).toInt

  def longValue: Long = numerator / denominator

  def compare(that: SmallRational): Int = {
    val left  = numerator * that.denominator
    val right = that.numerator * denominator
    java.lang.Long.compare(left, right)
  }

  def toRational: Rational = Rational(BigInt(numerator), BigInt(denominator))

  def toComplexSmallRational: ComplexSmallRational = ComplexSmallRational(this, SmallRational.ZERO)

  override def equals(x: Any): Boolean =
    x match {
      case r: SmallRational => numerator == r.numerator && denominator == r.denominator
      case r: Rational      => toRational == r
      case l: Long          => isWhole && numerator == l
      case i: Int           => isWhole && numerator == i.toLong
      case _                => false
    }

  override def toString: String =
    if (isZero) "0"
    else if (isWhole) numerator.toString
    else s"$numerator/$denominator"

  override def hashCode: Int = numerator.hashCode ^ denominator.hashCode
}

object SmallRational {
  private val SMALL_RATIONAL = """\s*(-?\d+)\s*/\s*(\d+)\s*""".r

  lazy val ZERO: SmallRational = SmallRational(0L)
  lazy val ONE: SmallRational  = SmallRational(1L)
  lazy val HALF: SmallRational = SmallRational(1L, 2L)

  private def gcd(a: Long, b: Long): Long = {
    @scala.annotation.tailrec
    def _gcd(a: Long, b: Long): Long = if (b == 0) a else _gcd(b, a % b)
    _gcd(a, b)
  }

  // Cross-platform overflow checking functions
  private[numbers] def checkMultiplyOverflow(a: Long, b: Long): Long = {
    if (a == 0L || b == 0L) return 0L
    if (a == 1L) return b
    if (b == 1L) return a
    if (a == -1L) {
      if (b == Long.MinValue) throw new ArithmeticException("SmallRational overflow")
      return -b
    }
    if (b == -1L) {
      if (a == Long.MinValue) throw new ArithmeticException("SmallRational overflow")
      return -a
    }

    // Check overflow for general case
    if (a > 0 && b > 0) {
      if (a > Long.MaxValue / b) throw new ArithmeticException("SmallRational overflow")
    } else if (a < 0 && b < 0) {
      if (a < Long.MaxValue / b) throw new ArithmeticException("SmallRational overflow")
    } else if (a > 0 && b < 0) {
      if (b < Long.MinValue / a) throw new ArithmeticException("SmallRational overflow")
    } else { // a < 0 && b > 0
      if (a < Long.MinValue / b) throw new ArithmeticException("SmallRational overflow")
    }

    a * b
  }

  private[numbers] def checkAddOverflow(a: Long, b: Long): Long = {
    if (a > 0 && b > 0) {
      if (a > Long.MaxValue - b) throw new ArithmeticException("SmallRational overflow")
    } else if (a < 0 && b < 0) {
      if (a < Long.MinValue - b) throw new ArithmeticException("SmallRational overflow")
    }
    a + b
  }

  private[numbers] def checkSubtractOverflow(a: Long, b: Long): Long = {
    if (a > 0 && b < 0) {
      if (a > Long.MaxValue + b) throw new ArithmeticException("SmallRational overflow")
    } else if (a < 0 && b > 0) {
      if (a < Long.MinValue + b) throw new ArithmeticException("SmallRational overflow")
    }
    a - b
  }

  // Safe operations that return Option to handle overflow
  def safeAdd(a: SmallRational, b: SmallRational): Option[SmallRational] =
    try Some(a.addExact(b))
    catch { case _: ArithmeticException => None }

  def safeMultiply(a: SmallRational, b: SmallRational): Option[SmallRational] =
    try Some(a.multiplyExact(b))
    catch { case _: ArithmeticException => None }

  def safeSubtract(a: SmallRational, b: SmallRational): Option[SmallRational] =
    try Some(a.subtractExact(b))
    catch { case _: ArithmeticException => None }

  def safeDivide(a: SmallRational, b: SmallRational): Option[SmallRational] =
    try Some(a.divideExact(b))
    catch { case _: ArithmeticException => None }

  def oneOver(d: Long): SmallRational = SmallRational(1L, d)

  def apply(s: String): SmallRational =
    s match {
      case SMALL_RATIONAL(n, d) =>
        val num = n.toLong
        val den = d.toLong
        new SmallRational(num, den)
      case _ => sys.error("not a rational")
    }

  def apply(n: Long, d: Long): SmallRational = new SmallRational(n, d)

  def apply(a: Long): SmallRational = new SmallRational(a, 1L)

  def apply(a: Int): SmallRational = new SmallRational(a.toLong, 1L)

  def unapply(z: Any): Option[(Long, Long)] =
    z match {
      case r: SmallRational => Some(r.numerator, r.denominator)
      case _                => None
    }

  // Factory methods for common fractions
  def half: SmallRational    = HALF
  def third: SmallRational   = SmallRational(1L, 3L)
  def quarter: SmallRational = SmallRational(1L, 4L)
  def fifth: SmallRational   = SmallRational(1L, 5L)

  implicit def int2smallRational(a: Int): SmallRational   = SmallRational(a)
  implicit def long2smallRational(a: Long): SmallRational = SmallRational(a)

  implicit def intdiv2smallRational(a: Int): IntDiv    = new IntDiv(a)
  implicit def longdiv2smallRational(a: Long): LongDiv = new LongDiv(a)

  class IntDiv(a: Int) {
    def \(b: Int): SmallRational = SmallRational(a.toLong, b.toLong)
  }

  class LongDiv(a: Long) {
    def \(b: Long): SmallRational = SmallRational(a, b)
  }

  implicit object smallRationalIsFractional extends SmallRationalIsFractional
}

trait SmallRationalIsFractional extends Fractional[SmallRational] {

  def plus(x: SmallRational, y: SmallRational): SmallRational = x + y

  def minus(x: SmallRational, y: SmallRational): SmallRational = x - y

  def times(x: SmallRational, y: SmallRational): SmallRational = x * y

  def div(x: SmallRational, y: SmallRational): SmallRational = x / y

  def negate(x: SmallRational): SmallRational = -x

  def parseString(s: String): Option[SmallRational] =
    try Some(SmallRational(s))
    catch { case _: Exception => None }

  def fromInt(x: Int): SmallRational = x

  def toInt(x: SmallRational): Int = x.intValue

  def toLong(x: SmallRational): Long = x.longValue

  def toFloat(x: SmallRational): Float = x.floatValue

  def toDouble(x: SmallRational): Double = x.doubleValue

  def compare(x: SmallRational, y: SmallRational): Int = x.compare(y)
}

// Complex number with SmallRational coefficients
case class ComplexSmallRational(re: SmallRational, im: SmallRational)
    extends AbstractComplexRational[SmallRational, ComplexSmallRational] {

  protected def fractional(a: SmallRational): Double = a.doubleValue

  protected def complex(re: SmallRational, im: SmallRational): ComplexSmallRational = ComplexSmallRational(re, im)

  protected def promote: ComplexDouble = ComplexDouble(re.doubleValue, im.doubleValue)

  protected def divide(a: SmallRational, b: SmallRational): SmallRational = a / b

  def zero: ComplexSmallRational = ComplexSmallRational.zero

  def one: ComplexSmallRational = ComplexSmallRational.one

  def i: ComplexSmallRational = ComplexSmallRational.i

  def doubleValue: Double = abs

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

}

object ComplexSmallRational {

  val i: ComplexSmallRational = ComplexSmallRational(SmallRational.ZERO, SmallRational.ONE)

  val zero: ComplexSmallRational = ComplexSmallRational(SmallRational.ZERO, SmallRational.ZERO)

  val one: ComplexSmallRational = ComplexSmallRational(SmallRational.ONE, SmallRational.ZERO)

  def apply(a: SmallRational): ComplexSmallRational = ComplexSmallRational(a, SmallRational.ZERO)

  implicit def smallRational2complex(a: SmallRational): ComplexSmallRational = ComplexSmallRational(a)

  implicit def int2complex(a: Int): ComplexSmallRational = ComplexSmallRational(SmallRational(a))

}
