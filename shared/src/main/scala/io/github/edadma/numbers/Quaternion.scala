package io.github.edadma.numbers

import Numeric.Implicits._

abstract class Quaternion[T: Numeric, F: Fractional, Q <: Quaternion[T, F, Q, P], P <: Quaternion[F, F, P, P]] extends Number {

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

  protected def quaternion(a: T): Q = quaternion(a, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].zero)

  protected def promote(a: F, b: F, c: F, d: F): P

  protected def promote(a: F): P = promote(a, implicitly[Numeric[F]].zero, implicitly[Numeric[F]].zero, implicitly[Numeric[F]].zero)

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

  //protected implicit def int2T(n: Int): T = implicitly[Numeric[T]].fromInt(n)

  lazy val im: Q = (this - conj) / 2

  lazy val ln: P = promote(_ln(abs)) + im.sgn * arg

  def exp: P = promote(_exp(fractional(a))) * (promote(_cos(im.abs)) + im.sgn * _sin(im.abs))

  def sin: P = promote(onef)

  def asin: P = promote(onef) //(-i).promote * (ix.promote + (one - this * this).sqrt).ln

  def sinh: P = (exp - (-this).exp) / 2

  def asinh: P = promote(onef) //(promote + (this * this + 1).sqrt).ln

  def cos: P = promote(onef) //(ix.exp + (-ix).exp) / 2

  def acos: P = promote(onef) //i.promote * (promote - i.promote * (one - this * this).sqrt).ln

  def acosh: P = (promote + (this + 1).sqrt * (this - 1).sqrt).ln

  def cosh: P = (exp + (-this).exp) / 2

  def tan: P = promote(onef) //((ix * 2).exp - 1) / i.promote / ((ix * 2).exp + 1)

  def atan: P = promote(onef) //i.promote * ((one - ix).ln - (one + ix).ln) / 2

  def tanh: P = (exp - (-this).exp) / (exp + (-this).exp)

  def atanh: P = promote(onef)

  def conj: Q = quaternion(a, -b, -c, -d)

  def ^(that: Q): P = (that.promote * ln).exp

  def ^(p: F): P = promote(onef) //pow(p)

//  def pow(p: F): P = {
//    val q = promote
//    val n = q.norm2
//    val a = q.arg
//    val pa = p * a
//
//    import Fractional.Implicits._
//
//    promote(_pow(n, p / implicitly[Fractional[F]].fromInt(2))) * promote(_cos(pa), _sin(pa))
//  }

  def ^(e: Int): Q

  def ^(e: BigInt): Q

  def +(that: Q): Q = quaternion(a + that.a, b + that.b, c + that.c, d + that.d)

  def +(that: T): Q = quaternion(a + that, b, c, d)

//  def +(that: F): P = promote(fractional(a) + that, fractional(b), fractional(c), fractional(d))

  def +(that: Int): Q = quaternion(a + implicitly[Numeric[T]].fromInt(that), b, c, d)

  def *(that: Q): Q =
    quaternion(a * that.a - b * that.b - c * that.c - d * that.d,
               a * that.b + b * that.a + c * that.d - d * that.c,
               a * that.c - b * that.d + c * that.a + d * that.b,
               a * that.d + b * that.c - c * that.b - d * that.a)

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
    promote(fdivide(fractional(a), that), fdivide(fractional(b), that), fdivide(fractional(c), that), fdivide(fractional(d), that))

  def /(that: Int): Q = {
    val n = implicitly[Numeric[T]].fromInt(that)

    quaternion(divide(a, n), divide(b, n), divide(c, n), divide(d, n))
  }

  def unary_- : Q = quaternion(-a, -b, -c, -d)

  def inverse: Q = conj / norm

  override def equals(o: Any): Boolean =
    o match {
      case q: Quaternion[T, F, Q, P] => a == q.a && b == q.b && c == q.c && d == q.d
      case _                         => false
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
            if (buf nonEmpty)
              buf += '+'

            buf += basis
          } else if (v == -onet)
            buf ++= s"-$basis"
          else if (implicitly[Numeric[T]].lt(v, zerot))
            buf ++= s"$v$basis"
          else {
            if (buf nonEmpty)
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
