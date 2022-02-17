package io.github.edadma.numbers

import java.math.{RoundingMode, MathContext}

import math._

class BigDecimalMath(val mc: MathContext) {

  def this(precision: Int) =
    this(new MathContext(precision, RoundingMode.HALF_EVEN))

  class Const(compute: => BigDecimal) {
    private var _value: BigDecimal = null

    def v = {
      if (_value == null || _value.mc != mc)
        _value = compute

      _value
    }
  }

  class ComplexBigDecimalConst(compute: => ComplexBigDecimal) {
    private var _value: ComplexBigDecimal = null

    def v: ComplexBigDecimal = {
      if (_value == null || _value.bdmath.mc != mc)
        _value = compute

      _value
    }
  }

  class QuaternionBigDecimalConst(compute: => QuaternionBigDecimal) {
    private var _value: QuaternionBigDecimal = null

    def v: QuaternionBigDecimal = {
      if (_value == null || _value.bdmath.mc != mc)
        _value = compute

      _value
    }
  }

  class IntConst(a: Int) extends Const(bigDecimal(a))

  class DoubleConst(a: Double) extends Const(bigDecimal(a))

  protected val ZERO = new IntConst(0)
  protected val QUARTER = new DoubleConst(.25)
  protected val ONE = new IntConst(1)
  protected val TWO = new IntConst(2)
  protected val THREE = new IntConst(3)
  protected val FOUR = new IntConst(4)

  val Pi = new Const(compute_pi)
  val E = new Const(compute_e)
  val LN2 = new Const(compute_ln2)
  val IC = new ComplexBigDecimalConst(ComplexBigDecimal(0, 1)(this))
  val IQ = new QuaternionBigDecimalConst(QuaternionBigDecimal(0, 1, 0, 0)(this))
  val JQ = new QuaternionBigDecimalConst(QuaternionBigDecimal(0, 0, 1, 0)(this))
  val KQ = new QuaternionBigDecimalConst(QuaternionBigDecimal(0, 0, 0, 1)(this))

  private[numbers] val ZEROC = new ComplexBigDecimalConst(ComplexBigDecimal(0, 0)(this))
  private[numbers] val ONEC = new ComplexBigDecimalConst(ComplexBigDecimal(1, 0)(this))
  private[numbers] val ZEROQ = new QuaternionBigDecimalConst(QuaternionBigDecimal(0, 0, 0, 0)(this))
  private[numbers] val ONEQ = new QuaternionBigDecimalConst(QuaternionBigDecimal(1, 0, 0, 0)(this))

  private[numbers] def bigDecimal(n: Int): BigDecimal = BigDecimal(n, mc)

  private[numbers] def bigDecimal(n: Double): BigDecimal = BigDecimal(n, mc)

  private[numbers] def inv(x: BigDecimal) = ONE.v / x

  private[numbers] def xx(x: BigDecimal) = x * x

  private def compute_ln2 = {
    var res = ZERO.v
    var p3 = THREE.v
    var p4 = FOUR.v
    var term = 1.0 / p3 + 1.0 / p4
    var k = 1

    while (term.scale < term.precision * 2) {
      res += term
      p3 *= 3
      p4 *= 4
      k += 1
      term = (1.0 / p3 + 1.0 / p4) / k
    }

    res.round(mc)
  }

  private def compute_pi = {
    var a = ONE.v
    var b = inv(BigDecimalMath.sqrt(TWO.v)(this))
    var t = QUARTER.v
    var x = 1

    while (a != b) {
      val y = a

      a = (a + b) / TWO.v
      b = BigDecimalMath.sqrt(b * y)(this)
      t = t - BigDecimal(x) * xx(y - a)
      x <<= 1
    }

    xx(a + b) / (FOUR.v * t)
  }

  private def compute_e = {
    var result = ZERO.v
    var term = TWO.v
    var d = ONE.v
    var i = TWO.v

    while (term.scale < term.precision * 2) {
      result += term
      d *= i
      i += 1
      term = inv(d)
    }

    result.round(mc)
  }

}

object BigDecimalMath {

  private val LN2D = math.log(2)

  object decimal128 {
    implicit val bdmath: BigDecimalMath = new BigDecimalMath(MathContext.DECIMAL128)
  }

  def ln(x: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    val p = bdmath.mc.getPrecision * math.log(10) / LN2D
    val m = ceil(p / 2 - math.log(x.toDouble) / LN2D).toInt
    val s = x * bdmath.TWO.v.pow(m)

    bdmath.Pi.v / (bdmath.TWO.v * agm(bdmath.ONE.v, bdmath.FOUR.v / s)) - m * bdmath.LN2.v
  }

  def agm(x: BigDecimal, y: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    def am(a: BigDecimal, b: BigDecimal) = (a + b) / 2

    def gm(a: BigDecimal, b: BigDecimal) = sqrt(a * b)

    def recur(an: BigDecimal, gn: BigDecimal): BigDecimal = {
      val anp1 = am(an, gn)
      val gnp1 = gm(an, gn)

      if ((anp1 - gnp1).abs <= an.ulp)
        anp1
      else
        recur(anp1, gnp1)
    }

    recur(am(x, y), gm(x, y))
  }

  def sqrt(x: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    var new_guess = x / bdmath.TWO.v
    var current_guess = x

    while (current_guess != new_guess) {
      current_guess = new_guess
      new_guess = (current_guess + x / current_guess) / bdmath.TWO.v
    }

    new_guess
  }

  def exp(a: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    val x_ = a
    var result = x_ + bdmath.ONE.v
    var n = x_
    var d = bdmath.ONE.v
    var term = x_
    var i = 2

    while (term.scale < term.precision * 2) {
      n *= x_
      d *= bdmath.bigDecimal(i)
      term = n / d
      result += term
      i += 1
    }

    result.round(bdmath.mc)
  }

  def log(b: BigDecimal, x: BigDecimal)(implicit bdmath: BigDecimalMath) =
    ln(x) / ln(b)

  def pow(x: BigDecimal, y: BigDecimal)(implicit bdmath: BigDecimalMath) =
    exp(y * ln(x))

  def pow(x: BigDecimal, y: Double)(implicit bdmath: BigDecimalMath) =
    exp(bdmath.bigDecimal(y) * ln(x))

  def sin(a: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    var term = a
    val x2 = bdmath.xx(a)
    var n = term
    var d = BigInt(1)
    var result = bdmath.ZERO.v
    var i = 3

    while (term.scale < term.precision * 2) {
      if ((i & 2) == 0)
        result -= term
      else
        result += term

      n *= x2
      d *= BigInt((i - 1) * i)
      term = n / BigDecimal(d)
      i += 2
    }

//		if (result.compareTo( ONE ) > 0)
//			return ONE;
//		else if (result.compareTo( NEG_ONE ) < 0)
//			return NEG_ONE;

    result.round(bdmath.mc)
  }

  def cos(a: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    var term = bdmath.ONE.v
    val x2 = bdmath.xx(a)
    var n = term
    var d = BigInt(1)
    var result = bdmath.ZERO.v
    var i = 2

    while (term.scale < term.precision * 2) {
      if ((i & 2) == 0)
        result -= term
      else
        result += term

      n *= x2
      d *= BigInt((i - 1) * i)
      term = n / BigDecimal(d)
      i += 2
    }

//		if (result.compareTo( ONE ) > 0)
//			return ONE;
//		else if (result.compareTo( NEG_ONE ) < 0)
//			return NEG_ONE;

    result.round(bdmath.mc)
  }

  def acos(a: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    var a_ = bdmath.ZERO.v
    var x1 = a
    var halves = bdmath.ONE.v

    require(a.abs <= bdmath.ONE.v, "acos() argument may not exceed one")

    while ({ halves /= bdmath.TWO.v; halves.scale < halves.precision * 2 }) if (x1.signum < 0) {
      x1 = bdmath.ONE.v - bdmath.TWO.v * bdmath.xx(x1)
      a_ += halves
    } else
      x1 = bdmath.TWO.v * bdmath.xx(x1) - bdmath.ONE.v

    (bdmath.Pi.v * a_).round(bdmath.mc)
  }

  def asin(a: BigDecimal)(implicit bdmath: BigDecimalMath) = {
    require(a.abs <= bdmath.ONE.v, "asin() argument may not exceed one")

    bdmath.Pi.v / 2 - acos(a)
  }

  def atan(a: BigDecimal)(implicit bdmath: BigDecimalMath) =
    a.signum * acos(bdmath.inv(sqrt(bdmath.xx(a) + bdmath.ONE.v)))

  def atan2(y: BigDecimal, x: BigDecimal)(implicit bdmath: BigDecimalMath) =
    if (x > 0)
      atan(y / x)
    else if (y >= 0 && x < 0)
      atan(y / x) + bdmath.Pi.v
    else if (y < 0 && x < 0)
      atan(y / x) - bdmath.Pi.v
    else if (y > 0 && x == 0)
      bdmath.Pi.v / 2
    else if (y < 0 && x == 0)
      -bdmath.Pi.v / 2
    else
      bdmath.bigDecimal(0)

  def atanh(x: BigDecimal)(implicit bdmath: BigDecimalMath) =
    (ln(bdmath.ONE.v + x) - ln(bdmath.ONE.v - x)) / bdmath.TWO.v

}
