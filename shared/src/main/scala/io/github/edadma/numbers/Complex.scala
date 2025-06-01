package io.github.edadma.numbers

import Numeric.Implicits._

abstract class Complex[T: Numeric, F: Fractional, C <: Complex[T, F, C, P], P <: Complex[F, F, P, P]] extends Number {

  val re: T
  val im: T

  protected def unsup: Nothing = sys.error("unsupported operation")

  protected def fractional(a: T): F

  protected def _floor(a: F): F

  protected def _ceil(a: F): F

  protected def _sqrt(a: F): F

  protected def _atan2(y: F, x: F): F

  protected def _ln(a: F): F

  protected def _exp(a: F): F

  protected def _sin(a: F): F

  protected def _cos(a: F): F

  protected def _pow(a: F, b: F): F

  protected def complex(re: T, im: T): C

  protected def complex(re: T): C = complex(re, implicitly[Numeric[T]].zero)

  protected def promote(re: F, im: F): P

  protected def promote(re: F): P = promote(re, implicitly[Numeric[F]].zero)

  protected def promote: P

  protected def divide(a: T, b: T): T

  def zero: C

  def one: C

  def i: C

  protected def ix: C = i * this

  def norm: T = re * re + im * im

  def abs: F = _sqrt(fractional(norm))

  def floor: P = promote(_floor(fractional(re)), _floor(fractional(im)))

  def ceil: P = promote(_ceil(fractional(re)), _ceil(fractional(im)))

  def arg: F = _atan2(fractional(im), fractional(re))

  def sqrt: P =
    this ^ implicitly[Fractional[F]].div(implicitly[Fractional[F]].fromInt(1), implicitly[Fractional[F]].fromInt(2))

  def ln: P = promote(_ln(abs), arg)

  def exp: P =
    promote(_exp(fractional(re))) * promote(_cos(fractional(im)), _sin(fractional(im)))

  def sin: P =
    (ix.exp - (-ix).exp) / promote(implicitly[Numeric[F]].zero, implicitly[Numeric[F]].fromInt(2))

  def asin: P = (-i).promote * (ix.promote + (one - this * this).sqrt).ln

  def sinh: P = (exp - (-this).exp) / 2

  def asinh: P = (promote + (this * this + 1).sqrt).ln

  def cos: P = (ix.exp + (-ix).exp) / 2

  def acos: P = i.promote * (promote - i.promote * (one - this * this).sqrt).ln

  def acosh: P = (promote + (this + 1).sqrt * (this - 1).sqrt).ln

  def cosh: P = (exp + (-this).exp) / 2

  def tan: P = sin / cos

  def atan: P = i.promote * ((one - ix).ln - (one + ix).ln) / 2

  def tanh: P = (exp - (-this).exp) / (exp + (-this).exp)

  def atanh: P = ((one + this).ln - (one - this).ln) / 2

  def conj: C = complex(re, -im)

  def ^(that: Complex[T, F, C, P]): P = (that.promote * ln).exp

  def ^(p: F): P = pow(p)

  def pow(p: F): P = {
    val c = promote
    val n = c.norm
    //		val lnn = _ln( n )
    val a  = c.arg
    val pa = p * a

    import Fractional.Implicits._

    promote(_pow(n, p / implicitly[Fractional[F]].fromInt(2))) * promote(_cos(pa), _sin(pa))
  }

  def ^(e: Int): C

  def ^(e: BigInt): C

  def +(that: Complex[T, F, C, P]): C = complex(re + that.re, im + that.im)

  def +(that: T): C = complex(re + that, im)

  def +(that: Int): C = complex(re + implicitly[Numeric[T]].fromInt(that), im)

  def *(that: Complex[T, F, C, P]): C =
    complex(re * that.re - im * that.im, im * that.re + re * that.im)

  def *(that: T): C = complex(re * that, im * that)

  def *(that: Int): C =
    complex(re * implicitly[Numeric[T]].fromInt(that), im * implicitly[Numeric[T]].fromInt(that))

  def -(that: Complex[T, F, C, P]): C = complex(re - that.re, im - that.im)

  def -(that: T): C = complex(re - that, im)

  def -(that: Int): C = complex(re - implicitly[Numeric[T]].fromInt(that), im)

  def /(that: Complex[T, F, C, P]): C =
    val denominatorNorm = that.norm

    if (denominatorNorm == implicitly[Numeric[T]].zero) sys.error("division by zero")
    complex(
      divide(re * that.re + im * that.im, denominatorNorm),
      divide(im * that.re - re * that.im, denominatorNorm),
    )

  def /(that: T): C =
    if (that == implicitly[Numeric[T]].zero) sys.error("division by zero")
    complex(divide(re, that), divide(im, that))

  def /(that: Int): C =
    val thatT = implicitly[Numeric[T]].fromInt(that)

    if (thatT == implicitly[Numeric[T]].zero) sys.error("division by zero")
    complex(divide(re, thatT), divide(im, thatT))

  def unary_- : C = complex(-re, -im)

  def inverse: C =
    val n = norm

    if (n == implicitly[Numeric[T]].zero) sys.error("division by zero")
    conj / n

  override def equals(o: Any): Boolean =
    o match {
      case z: Complex[T, F, C, P] @unchecked => re == z.re && im == z.im
      case _                                 => false
    }

  override def hashCode: Int = re.hashCode ^ im.hashCode

  override def toString: String = {
    val zero = implicitly[Numeric[T]].zero
    val one  = implicitly[Numeric[T]].one

    if (im == zero)
      re.toString
    else if (re == zero) {
      if (im == one)
        "i"
      else if (im == -one)
        "-i"
      else
        s"${im}i"
    } else if (im == one)
      s"$re+i"
    else if (im == -one)
      s"$re-i"
    else if (implicitly[Numeric[T]].lt(im, zero))
      s"$re${im}i"
    else
      s"$re+${im}i"
  }

}
