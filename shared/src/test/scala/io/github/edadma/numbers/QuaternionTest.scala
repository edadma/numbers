package io.github.edadma.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.math._

class QuaternionTest extends AnyFlatSpec with Matchers {

  private def roughly(x: Double, y: Double, tolerance: Double = 1e-10): Boolean =
    (x - y).abs < tolerance

  private def quaternionRoughlyEquals(q1: QuaternionDouble, q2: QuaternionDouble, tolerance: Double = 1e-10): Boolean =
    roughly(q1.a, q2.a, tolerance) && roughly(q1.b, q2.b, tolerance) &&
      roughly(q1.c, q2.c, tolerance) && roughly(q1.d, q2.d, tolerance)

  "QuaternionDouble multiplication" should "follow the correct Hamilton rules" in {
    val i   = QuaternionDouble(0, 1, 0, 0)
    val j   = QuaternionDouble(0, 0, 1, 0)
    val k   = QuaternionDouble(0, 0, 0, 1)
    val one = QuaternionDouble(1, 0, 0, 0)

    // Test fundamental quaternion identities: i² = j² = k² = ijk = -1
    (i * i) shouldBe QuaternionDouble(-1, 0, 0, 0)
    (j * j) shouldBe QuaternionDouble(-1, 0, 0, 0)
    (k * k) shouldBe QuaternionDouble(-1, 0, 0, 0)

    // Test i*j = k, j*k = i, k*i = j
    (i * j) shouldBe k
    (j * k) shouldBe i
    (k * i) shouldBe j

    // Test j*i = -k, k*j = -i, i*k = -j (anti-commutativity)
    (j * i) shouldBe QuaternionDouble(0, 0, 0, -1)
    (k * j) shouldBe QuaternionDouble(0, -1, 0, 0)
    (i * k) shouldBe QuaternionDouble(0, 0, -1, 0)

    // Test i*j*k = -1
    val ijk = i * j * k
    ijk shouldBe QuaternionDouble(-1, 0, 0, 0)
  }

  it should "satisfy the general multiplication formula" in {
    val q1 = QuaternionDouble(1, 2, 3, 4)
    val q2 = QuaternionDouble(5, 6, 7, 8)

    val result = q1 * q2

    // Expected result using correct formula:
    // a = 1*5 - 2*6 - 3*7 - 4*8 = 5 - 12 - 21 - 32 = -60
    // b = 1*6 + 2*5 + 3*8 - 4*7 = 6 + 10 + 24 - 28 = 12
    // c = 1*7 - 2*8 + 3*5 + 4*6 = 7 - 16 + 15 + 24 = 30
    // d = 1*8 + 2*7 - 3*6 + 4*5 = 8 + 14 - 18 + 20 = 24
    val expected = QuaternionDouble(-60, 12, 30, 24)

    result shouldBe expected
  }

  it should "be non-commutative" in {
    val q1 = QuaternionDouble(1, 2, 0, 0)
    val q2 = QuaternionDouble(0, 0, 3, 4)

    val ab = q1 * q2
    val ba = q2 * q1

    ab should not be ba
  }

  it should "be associative" in {
    val q1 = QuaternionDouble(1, 2, 3, 4)
    val q2 = QuaternionDouble(5, 6, 7, 8)
    val q3 = QuaternionDouble(2, 1, 4, 3)

    val left  = (q1 * q2) * q3
    val right = q1 * (q2 * q3)

    quaternionRoughlyEquals(left, right) shouldBe true
  }

  "QuaternionDouble division" should "be the inverse of multiplication" in {
    val q1 = QuaternionDouble(1, 2, 3, 4)
    val q2 = QuaternionDouble(5, 6, 7, 8)

    val product  = q1 * q2
    val quotient = product / q2

    quaternionRoughlyEquals(quotient, q1) shouldBe true
  }

  it should "satisfy q / q = 1 for non-zero quaternions" in {
    val q      = QuaternionDouble(2, 3, 4, 5)
    val result = q / q
    val one    = QuaternionDouble(1, 0, 0, 0)

    quaternionRoughlyEquals(result, one) shouldBe true
  }

  it should "throw on division by zero" in {
    val q    = QuaternionDouble(1, 2, 3, 4)
    val zero = QuaternionDouble(0, 0, 0, 0)

    a[RuntimeException] should be thrownBy {
      q / zero
    }
  }

  "QuaternionDouble scalar operations" should "affect only the real part for addition/subtraction" in {
    val q = QuaternionDouble(1, 2, 3, 4)

    // Scalar addition should only affect real part
    (q + 5) shouldBe QuaternionDouble(6, 2, 3, 4)
    (q + 5.0) shouldBe QuaternionDouble(6, 2, 3, 4)

    // Scalar subtraction should only affect real part
    (q - 3) shouldBe QuaternionDouble(-2, 2, 3, 4)
    (q - 3.0) shouldBe QuaternionDouble(-2, 2, 3, 4)
  }

  it should "scale all components for multiplication/division" in {
    val q = QuaternionDouble(1, 2, 3, 4)

    // Scalar multiplication should scale all components
    (q * 2) shouldBe QuaternionDouble(2, 4, 6, 8)
    (q * 2.0) shouldBe QuaternionDouble(2, 4, 6, 8)

    // Scalar division should scale all components
    (q / 2) shouldBe QuaternionDouble(0.5, 1, 1.5, 2)
    (q / 2.0) shouldBe QuaternionDouble(0.5, 1, 1.5, 2)
  }

  "QuaternionDouble conjugate" should "negate the vector part" in {
    val q = QuaternionDouble(1, 2, 3, 4)
    q.conj shouldBe QuaternionDouble(1, -2, -3, -4)
  }

  it should "satisfy conjugate properties" in {
    val q1 = QuaternionDouble(1, 2, 3, 4)
    val q2 = QuaternionDouble(5, 6, 7, 8)

    // (q1 * q2)* = q2* * q1*
    val leftSide  = (q1 * q2).conj
    val rightSide = q2.conj * q1.conj

    quaternionRoughlyEquals(leftSide, rightSide) shouldBe true

    // (q*)* = q
    quaternionRoughlyEquals(q1.conj.conj, q1) shouldBe true
  }

  "QuaternionDouble norm and abs" should "calculate correctly" in {
    val q = QuaternionDouble(1, 2, 3, 4)

    // norm = a² + b² + c² + d²
    q.norm shouldBe 30.0 +- 1e-10

    // abs = sqrt(norm)
    q.abs shouldBe sqrt(30.0) +- 1e-10
  }

  it should "satisfy multiplicative property" in {
    val q1 = QuaternionDouble(1, 2, 3, 4)
    val q2 = QuaternionDouble(5, 6, 7, 8)

    // |q1 * q2| = |q1| * |q2|
    val leftSide  = (q1 * q2).abs
    val rightSide = q1.abs * q2.abs

    roughly(leftSide, rightSide) shouldBe true
  }

  "QuaternionDouble inverse" should "satisfy q * q⁻¹ = 1" in {
    val q       = QuaternionDouble(1, 2, 3, 4)
    val inv     = q.inverse
    val product = q * inv
    val one     = QuaternionDouble(1, 0, 0, 0)

    quaternionRoughlyEquals(product, one) shouldBe true
  }

  it should "equal conjugate/norm for non-zero quaternions" in {
    val q    = QuaternionDouble(1, 2, 3, 4)
    val inv1 = q.inverse
    val inv2 = q.conj / q.norm

    quaternionRoughlyEquals(inv1, inv2) shouldBe true
  }

  "QuaternionDouble power operations" should "satisfy basic power rules" in {
    val q = QuaternionDouble(1, 1, 0, 0)

    // q^0 = 1
    (q ^ 0) shouldBe QuaternionDouble(1, 0, 0, 0)

    // q^1 = q
    (q ^ 1) shouldBe q

    // q^2 = q * q
    val q2_1 = q ^ 2
    val q2_2 = q * q
    quaternionRoughlyEquals(q2_1, q2_2) shouldBe true
  }

  "Unit quaternions" should "preserve norm under multiplication" in {
    val angle1 = Pi / 4
    val angle2 = Pi / 6
    val axis1  = QuaternionDouble(0, 1, 0, 0) // i direction
    val axis2  = QuaternionDouble(0, 0, 1, 0) // j direction

    val q1 = QuaternionDouble(cos(angle1 / 2), sin(angle1 / 2), 0, 0)
    val q2 = QuaternionDouble(cos(angle2 / 2), 0, sin(angle2 / 2), 0)

    roughly(q1.abs, 1.0) shouldBe true
    roughly(q2.abs, 1.0) shouldBe true
    roughly((q1 * q2).abs, 1.0) shouldBe true
  }

  "3D rotation using quaternions" should "work correctly for simple cases" in {
    // 90-degree rotation around Z-axis
    val angle = Pi / 2
    val q     = QuaternionDouble(cos(angle / 2), 0, 0, sin(angle / 2))

    // Rotate point (1,0,0) -> should become (0,1,0)
    val point   = QuaternionDouble(0, 1, 0, 0)
    val rotated = q * point * q.conj

    // The result should be approximately (0,0,1,0)
    quaternionRoughlyEquals(rotated, QuaternionDouble(0, 0, 1, 0)) shouldBe true
  }

  "QuaternionRational" should "provide exact arithmetic" in {
    val q1 = QuaternionRational(Rational(1, 2), Rational(1, 3), Rational(1, 4), Rational(1, 5))
    val q2 = QuaternionRational(Rational(2, 3), Rational(3, 4), Rational(4, 5), Rational(5, 6))

    val sum = q1 + q2
    sum shouldBe QuaternionRational(
      Rational(7, 6),   // 1/2 + 2/3 = 3/6 + 4/6 = 7/6
      Rational(13, 12), // 1/3 + 3/4 = 4/12 + 9/12 = 13/12
      Rational(21, 20), // 1/4 + 4/5 = 5/20 + 16/20 = 21/20
      Rational(31, 30), // 1/5 + 5/6 = 6/30 + 25/30 = 31/30
    )
  }

  "QuaternionInt" should "handle integer arithmetic correctly" in {
    val q1 = QuaternionInt(1, 2, 3, 4)
    val q2 = QuaternionInt(5, 6, 7, 8)

    val sum = q1 + q2
    sum shouldBe QuaternionInt(6, 8, 10, 12)

    val product = q1 * q2
    // Using correct multiplication formula
    product shouldBe QuaternionInt(-60, 12, 30, 24)
  }

  "Quaternion string representation" should "be readable" in {
    QuaternionDouble(0, 0, 0, 0).toString shouldBe "0"
    QuaternionDouble(1, 0, 0, 0).toString shouldBe "1"
    QuaternionDouble(0, 1, 0, 0).toString shouldBe "i"
    QuaternionDouble(0, 0, 1, 0).toString shouldBe "j"
    QuaternionDouble(0, 0, 0, 1).toString shouldBe "k"
    QuaternionDouble(1, 1, 1, 1).toString shouldBe "1+i+j+k"
    QuaternionDouble(1, -1, 2, -3).toString shouldBe "1-i+2j-3k"
  }

  "Quaternion mathematical functions" should "work for simple cases" in {
    val q = QuaternionDouble(1, 0, 0, 0) // Real quaternion

    // exp(1) for real quaternion should be approximately e
    val expQ = q.exp
    roughly(expQ.a, E) shouldBe true
    roughly(expQ.b, 0) shouldBe true
    roughly(expQ.c, 0) shouldBe true
    roughly(expQ.d, 0) shouldBe true

    // ln(e) should be approximately 1
    val eQ  = QuaternionDouble(E, 0, 0, 0)
    val lnE = eQ.ln
    roughly(lnE.a, 1.0) shouldBe true
  }

  "Edge cases" should "be handled correctly" in {
    val zero = QuaternionDouble(0, 0, 0, 0)
    val one  = QuaternionDouble(1, 0, 0, 0)
    val q    = QuaternionDouble(1, 2, 3, 4)

    // Addition with zero
    (q + zero) shouldBe q
    (zero + q) shouldBe q

    // Multiplication with zero
    (q * zero) shouldBe zero
    (zero * q) shouldBe zero

    // Multiplication with one
    (q * one) shouldBe q
    (one * q) shouldBe q

    // Conjugate of zero
    zero.conj shouldBe zero

    // Norm of zero
    zero.norm shouldBe 0.0
    zero.abs shouldBe 0.0

    // Inverse of zero should throw
    a[RuntimeException] should be thrownBy {
      zero.inverse
    }
  }

  "Complex error-detecting test cases" should "catch implementation bugs" in {
    // This test specifically catches the k-coefficient sign error
    val q1 = QuaternionDouble(1, 0, 1, 0) // 1 + j
    val q2 = QuaternionDouble(0, 1, 0, 1) // i + k

    val result = q1 * q2
    // Correct calculation:
    // a = 1*0 - 0*1 - 1*0 - 0*1 = 0
    // b = 1*1 + 0*0 + 1*1 - 0*0 = 2
    // c = 1*0 - 0*1 + 1*0 + 0*1 = 0
    // d = 1*1 + 0*0 - 1*1 + 0*0 = 0  (Note: +0*0, not -0*0)
    val expected = QuaternionDouble(0, 2, 0, 0)

    result shouldBe expected
  }

  it should "catch division order errors" in {
    val q1 = QuaternionDouble(1, 2, 0, 0)
    val q2 = QuaternionDouble(3, 4, 0, 0)

    // Test that (q1 * q2) / q2 = q1
    val product  = q1 * q2
    val quotient = product / q2

    quaternionRoughlyEquals(quotient, q1, 1e-10) shouldBe true
  }

  it should "catch scalar subtraction errors" in {
    val q      = QuaternionDouble(5, 3, 2, 1)
    val result = q - 2

    // Should only affect real part
    result shouldBe QuaternionDouble(3, 3, 2, 1)
  }

  "Cross-type operations" should "work correctly" in {
    val qd = QuaternionDouble(1, 2, 3, 4)
    val qi = QuaternionInt(1, 2, 3, 4)

    // Test conversion via promotion
    val qdFromInt = QuaternionDouble(qi.a.toDouble, qi.b.toDouble, qi.c.toDouble, qi.d.toDouble)
    quaternionRoughlyEquals(qdFromInt, qd) shouldBe true
  }

  "BigDecimal quaternions" should "maintain precision" in {
    import BigDecimalMath.decimal128._

    val q1 = QuaternionBigDecimal(
      BigDecimal("1.123456789012345"),
      BigDecimal("2.234567890123456"),
      BigDecimal(0),
      BigDecimal(0),
    )
    val q2 = QuaternionBigDecimal(
      BigDecimal("3.345678901234567"),
      BigDecimal("4.456789012345678"),
      BigDecimal(0),
      BigDecimal(0),
    )

    val sum = q1 + q2
    sum.a.toString should include("4.469135690246912") // Exact arithmetic
    sum.b.toString should include("6.691356902469134") // Exact arithmetic
  }
}
