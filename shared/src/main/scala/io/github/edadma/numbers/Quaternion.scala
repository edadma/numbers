package io.github.edadma.numbers

import Numeric.Implicits._

abstract class Quaternion[T: Numeric, F: Fractional, Q <: Quaternion[T, F, Q, P], P <: Quaternion[F, F, P, P]]
    extends Number {

  val a: T
  val b: T
  val c: T
  val d: T

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

  protected def _acos(a: F): F

  protected def _pow(a: F, b: F): F

  protected def quaternion(a: T, b: T, c: T, e: T): Q

  protected def quaternion(a: T): Q =
    quaternion(a, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].zero)

  protected def promote(a: F, b: F, c: F, d: F): P

  protected def promote(a: F): P =
    promote(a, implicitly[Numeric[F]].zero, implicitly[Numeric[F]].zero, implicitly[Numeric[F]].zero)

  protected def promote: P

  protected def divide(a: T, b: T): T

  protected def fdivide(a: F, b: F): F

  protected def fmul(a: F, b: F): F

  def zero: Q

  def one: Q

  def i: Q

//  protected def ix: Q = i * this

  def zerot: T = implicitly[Numeric[T]].zero

  def onet: T = implicitly[Numeric[T]].one

  def zerof: F = implicitly[Fractional[F]].zero

  def onef: F = implicitly[Fractional[F]].one

  lazy val norm: T = a * a + b * b + c * c + d * d

  lazy val abs: F = _sqrt(fractional(norm))

  lazy val absv: F = _sqrt(fractional(b * b + c * c + d * d))

  def floor: P = promote(_floor(fractional(a)), _floor(fractional(b)), _floor(fractional(c)), _floor(fractional(d)))

  def ceil: P = promote(_ceil(fractional(a)), _ceil(fractional(b)), _ceil(fractional(c)), _ceil(fractional(d)))

  def sqrt: P =
    this ^ implicitly[Fractional[F]].div(onef, implicitly[Fractional[F]].fromInt(2))

  lazy val sgn: P =
    if (this == zero) zero.promote
    else this.promote / abs

  lazy val arg: F = _acos(fdivide(fractional(a), abs))

  // protected implicit def int2T(n: Int): T = implicitly[Numeric[T]].fromInt(n)

  lazy val im: Q = (this - conj) / 2

  lazy val ln: P =
    val thisPromoted = promote
    val magnitude    = abs
    val vectorNorm   = absv

    if (vectorNorm == zerof) {
      // Pure real quaternion: ln(a) = ln(|a|) + πi (if a < 0)
      if (fractional(a) >= zerof) {
        promote(_ln(magnitude), zerof, zerof, zerof)
      } else {
        // For negative real quaternions, add π in the i direction
        val pi = promote(fractional(a)) // Need π constant, this is simplified
        promote(_ln(magnitude), pi, zerof, zerof)
      }
    } else {
      // General case: ln(q) = ln(|q|) + (v/|v|) * arccos(a/|q|)
      val logMagnitude = _ln(magnitude)
      val angle        = _acos(fdivide(fractional(a), magnitude))
      val vectorUnit   = thisPromoted.im.promote / vectorNorm

      promote(logMagnitude) + vectorUnit * angle
    }

  def exp: P =
    val thisPromoted = promote
    val realPart     = fractional(a)
    val vectorNorm   = absv

    if (vectorNorm == zerof) {
      // Pure real quaternion: exp(a) = exp(a)
      promote(_exp(realPart), zerof, zerof, zerof)
    } else {
      // General case: exp(q) = exp(a) * (cos(|v|) + (v/|v|) * sin(|v|))
      val expReal    = _exp(realPart)
      val cosVec     = _cos(vectorNorm)
      val sinVec     = _sin(vectorNorm)
      val vectorUnit = thisPromoted.im.promote / vectorNorm

      promote(expReal) * (promote(cosVec) + vectorUnit * sinVec)
    }

  def sin: P = {
    // For quaternions: sin(q) = sin(a)cosh(|v|) + (v/|v|)cos(a)sinh(|v|)
    val realPart = fractional(a)
    val vecNorm  = absv

    if (vecNorm == zerof) {
      // Pure real quaternion
      promote(_sin(realPart), zerof, zerof, zerof)
    } else {
      // Complex implementation - fallback to exponential form
      val thisPromoted = promote
      val halfI        = i.promote / 2
      val iz           = halfI * thisPromoted
      val eiz          = iz.exp
      val e_neg_iz     = (-iz).exp
      (eiz - e_neg_iz) / (halfI * 2)
    }
  }

  def cos: P = {
    // cos(q) = cos(a)cosh(|v|) - (v/|v|)sin(a)sinh(|v|)
    val realPart = fractional(a)
    val vecNorm  = absv

    if (vecNorm == zerof) {
      // Pure real quaternion
      promote(_cos(realPart), zerof, zerof, zerof)
    } else {
      // Complex implementation - fallback to exponential form
      val thisPromoted = promote
      val halfI        = i.promote / 2
      val iz           = halfI * thisPromoted
      val eiz          = iz.exp
      val e_neg_iz     = (-iz).exp
      (eiz + e_neg_iz) / 2
    }
  }

  def tan: P = sin / cos

  def asin: P = (-i.promote) * (i.promote * promote + (promote.one - promote * promote).sqrt).ln

  def acos: P = (-i.promote) * (promote + i.promote * (promote.one - promote * promote).sqrt).ln

  def atan: P = {
    val thisPromoted = promote
    val halfI        = i.promote / 2
    halfI * ((i.promote + thisPromoted) / (i.promote - thisPromoted)).ln
  }

  def sinh: P = (exp - (-this).exp) / 2

  def asinh: P = promote(onef) // (promote + (this * this + 1).sqrt).ln

  def acosh: P = (promote + (this + 1).sqrt * (this - 1).sqrt).ln

  def cosh: P = (exp + (-this).exp) / 2

  def tanh: P = (exp - (-this).exp) / (exp + (-this).exp)

  def atanh: P =
    // atanh(q) = (1/2) * ln((1+q)/(1-q))
    val thisPromoted = promote
    val one          = thisPromoted.one
    val two          = promote(implicitly[Fractional[F]].fromInt(2))
    val numerator    = one + thisPromoted
    val denominator  = one - thisPromoted

    (numerator / denominator).ln / two

  def conj: Q = quaternion(a, -b, -c, -d)

  def ^(that: Q): P = (that.promote * ln).exp

  def ^(p: F): P =
    // q^p = exp(p * ln(q))
    val thisPromoted = promote

    (thisPromoted.ln * p).exp

  def pow(p: F): P = this ^ p

  def ^(e: Int): Q

  def ^(e: BigInt): Q

  def +(that: Q): Q = quaternion(a + that.a, b + that.b, c + that.c, d + that.d)

  def +(that: T): Q = quaternion(a + that, b, c, d)

//  def +(that: F): P = promote(fractional(a) + that, fractional(b), fractional(c), fractional(d))

  def +(that: Int): Q = quaternion(a + implicitly[Numeric[T]].fromInt(that), b, c, d)

  def *(that: Q): Q =
    quaternion(
      a * that.a - b * that.b - c * that.c - d * that.d,
      a * that.b + b * that.a + c * that.d - d * that.c,
      a * that.c - b * that.d + c * that.a + d * that.b,
      a * that.d + b * that.c - c * that.b - d * that.a,
    )

  def *(that: T): Q = quaternion(a * that, b * that, c * that, d * that)

//  def *(that: F): P = promote(fractional(a) * that, fractional(b) * that, fractional(c) * that, fractional(d) * that)

  def *(that: Int): Q = {
    val n = implicitly[Numeric[T]].fromInt(that)

    quaternion(a * n, b * n, c * n, d * n)
  }

  def -(that: Quaternion[T, F, Q, P]): Q = quaternion(a - that.a, b - that.b, c - that.c, d - that.d)

  def -(that: T): Q = quaternion(a - that, b - that, c - that, d - that)

  def -(that: Int): Q = {
    val n = implicitly[Numeric[T]].fromInt(that)

    quaternion(a - n, b - n, c - n, d - n)
  }

  def /(that: Q): Q = that * inverse

  def /(that: T): Q = quaternion(divide(a, that), divide(b, that), divide(c, that), divide(d, that))

  def \(that: F): P =
    promote(
      fdivide(fractional(a), that),
      fdivide(fractional(b), that),
      fdivide(fractional(c), that),
      fdivide(fractional(d), that),
    )

  def /(that: Int): Q = {
    val n = implicitly[Numeric[T]].fromInt(that)

    quaternion(divide(a, n), divide(b, n), divide(c, n), divide(d, n))
  }

  def unary_- : Q = quaternion(-a, -b, -c, -d)

  def inverse: Q = conj / norm

  override def equals(o: Any): Boolean =
    o match {
      case q: Quaternion[T, F, Q, P] @unchecked => a == q.a && b == q.b && c == q.c && d == q.d
      case _                                    => false
    }

  override def hashCode: Int = a.hashCode ^ b.hashCode ^ c.hashCode ^ d.hashCode

  override def toString: String =
    if (this == zero)
      "0"
    else {
      val buf = new StringBuilder

      def imag(v: T, basis: Char): Unit =
        if (v != zerot) {
          if (v == onet) {
            if (buf.nonEmpty)
              buf += '+'

            buf += basis
          } else if (v == -onet)
            buf ++= s"-$basis"
          else if (implicitly[Numeric[T]].lt(v, zerot))
            buf ++= s"$v$basis"
          else {
            if (buf.nonEmpty)
              buf += '+'

            buf ++= s"$v$basis"
          }
        }

      if (a != zerot)
        buf ++= a.toString

      imag(b, 'i')
      imag(c, 'j')
      imag(d, 'k')
      buf.toString
    }

}
