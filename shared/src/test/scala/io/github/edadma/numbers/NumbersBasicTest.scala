package io.github.edadma.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumbersBasicTest extends AnyFlatSpec with Matchers {

  "Rational" should "handle basic arithmetic" in {
    val r1 = Rational(1, 2)
    val r2 = Rational(1, 3)

    (r1 + r2) shouldBe Rational(5, 6)
    (r1 - r2) shouldBe Rational(1, 6)
    (r1 * r2) shouldBe Rational(1, 6)
    (r1 / r2) shouldBe Rational(3, 2)
  }

  it should "handle special values" in {
    Rational.ZERO shouldBe Rational(0)
    Rational.ONE shouldBe Rational(1)
    Rational(4, 2) shouldBe Rational(2) // reduction
  }

  it should "handle power operations" in {
    Rational(2, 3) ^ 2 shouldBe Rational(4, 9)
    Rational(2) ^ 3 shouldBe Rational(8)
  }

  "ComplexDouble" should "handle basic arithmetic" in {
    val c1 = ComplexDouble(1, 2)
    val c2 = ComplexDouble(3, 4)

    (c1 + c2) shouldBe ComplexDouble(4, 6)
    (c1 - c2) shouldBe ComplexDouble(-2, -2)
    (c1 * c2) shouldBe ComplexDouble(-5, 10) // (1+2i)(3+4i) = 3+4i+6i+8i² = 3+10i-8 = -5+10i
  }

  "ComplexDoubleIsFractional parser" should "parse complex numbers correctly" in {
    import ComplexDoubleIsFractional.complexDoubleIsFractional

    // Basic positive case
    complexDoubleIsFractional.parseString("3+4i") shouldBe Some(ComplexDouble(3, 4))

    // The bug case - negative imaginary
    complexDoubleIsFractional.parseString("3-4i") shouldBe Some(ComplexDouble(3, -4))

    // Unit imaginary cases
    complexDoubleIsFractional.parseString("3+i") shouldBe Some(ComplexDouble(3, 1))
    complexDoubleIsFractional.parseString("3-i") shouldBe Some(ComplexDouble(3, -1))

    // Pure imaginary
    complexDoubleIsFractional.parseString("4i") shouldBe Some(ComplexDouble(0, 4))
    complexDoubleIsFractional.parseString("-4i") shouldBe Some(ComplexDouble(0, -4))
    complexDoubleIsFractional.parseString("i") shouldBe Some(ComplexDouble(0, 1))
    complexDoubleIsFractional.parseString("-i") shouldBe Some(ComplexDouble(0, -1))

    // Decimal cases
    complexDoubleIsFractional.parseString("3.5+2.7i") shouldBe Some(ComplexDouble(3.5, 2.7))
    complexDoubleIsFractional.parseString("1.2-3.4i") shouldBe Some(ComplexDouble(1.2, -3.4))

    // Scientific notation
    complexDoubleIsFractional.parseString("1e2+3e-1i") shouldBe Some(ComplexDouble(100, 0.3))

    // Invalid cases
    complexDoubleIsFractional.parseString("not-a-complex") shouldBe None
    complexDoubleIsFractional.parseString("3+4j") shouldBe None // j not i
  }

  it should "handle special values and operations" in {
    ComplexDouble.zero shouldBe ComplexDouble(0, 0)
    ComplexDouble.one shouldBe ComplexDouble(1, 0)
    ComplexDouble.i shouldBe ComplexDouble(0, 1)

    ComplexDouble.i * ComplexDouble.i shouldBe ComplexDouble(-1, 0)
  }

  it should "handle conjugate and absolute value" in {
    val c = ComplexDouble(3, 4)
    c.conj shouldBe ComplexDouble(3, -4)
    c.abs shouldBe 5.0 +- 0.001
  }

  "ComplexInt" should "handle basic operations" in {
    val c1 = ComplexInt(1, 2)
    val c2 = ComplexInt(3, 4)

    (c1 + c2) shouldBe ComplexInt(4, 6)
    (c1 * c2) shouldBe ComplexInt(-5, 10)
    c1.conj shouldBe ComplexInt(1, -2)
  }

  "ComplexRational" should "handle basic operations" in {
    val c1 = ComplexRational(Rational(1, 2), Rational(1, 3))
    val c2 = ComplexRational(Rational(1, 4), Rational(1, 6))

    (c1 + c2) shouldBe ComplexRational(Rational(3, 4), Rational(1, 2))
    c1.conj shouldBe ComplexRational(Rational(1, 2), Rational(-1, 3))
  }

  "ComplexBigInt" should "handle basic operations" in {
    val c1 = ComplexBigInt(BigInt(1), BigInt(2))
    val c2 = ComplexBigInt(BigInt(3), BigInt(4))

    (c1 + c2) shouldBe ComplexBigInt(BigInt(4), BigInt(6))
    (c1 - c2) shouldBe ComplexBigInt(BigInt(-2), BigInt(-2))
    (c1 * c2) shouldBe ComplexBigInt(BigInt(-5), BigInt(10))
    (-c1) shouldBe ComplexBigInt(BigInt(-1), BigInt(-2))
    c1.conj shouldBe ComplexBigInt(BigInt(1), BigInt(-2))
  }

  it should "handle large numbers" in {
    val large = BigInt("123456789012345678901234567890")
    val c1    = ComplexBigInt(large, BigInt(1))
    val c2    = ComplexBigInt(BigInt(2), large)

    val result = c1 + c2
    result.re shouldBe (large + 2)
    result.im shouldBe (large + 1)

    // Test scalar multiplication
    (c1 * 2) shouldBe ComplexBigInt(large * 2, BigInt(2))
  }

  it should "handle power operations" in {
    val c = ComplexBigInt(BigInt(2), BigInt(0))
    (c ^ 3) shouldBe ComplexBigInt(BigInt(8), BigInt(0))

    // i^2 = -1
    (ComplexBigInt.i ^ 2) shouldBe ComplexBigInt(BigInt(-1), BigInt(0))

    // Test special values
    ComplexBigInt.zero shouldBe ComplexBigInt(BigInt(0), BigInt(0))
    ComplexBigInt.one shouldBe ComplexBigInt(BigInt(1), BigInt(0))
    ComplexBigInt.i shouldBe ComplexBigInt(BigInt(0), BigInt(1))
  }

  "ComplexBigDecimal" should "handle basic operations" in {
    import BigDecimalMath.decimal128._

    val c1 = ComplexBigDecimal(BigDecimal(1), BigDecimal(2))
    val c2 = ComplexBigDecimal(BigDecimal(3), BigDecimal(4))

    (c1 + c2) shouldBe ComplexBigDecimal(BigDecimal(4), BigDecimal(6))
    (c1 * c2).re shouldBe BigDecimal(-5) +- BigDecimal("0.001")
    (c1 * c2).im shouldBe BigDecimal(10) +- BigDecimal("0.001")
    c1.conj shouldBe ComplexBigDecimal(BigDecimal(1), BigDecimal(-2))
  }

  it should "handle high precision arithmetic" in {
    import BigDecimalMath.decimal128._

    val precise1 = BigDecimal("1.123456789012345678901234567890")
    val precise2 = BigDecimal("2.987654321098765432109876543210")

    val c1 = ComplexBigDecimal(precise1, BigDecimal(0))
    val c2 = ComplexBigDecimal(precise2, BigDecimal(0))

    val result = c1 + c2
    result.re shouldBe BigDecimal("4.111111110111111111011111111100") +- BigDecimal("0.000000000000000000000000000001")
  }

  it should "handle mathematical functions" in {
    import BigDecimalMath.decimal128._

    val c = ComplexBigDecimal(BigDecimal(1), BigDecimal(0))

    // Basic properties should hold
    c.conj shouldBe ComplexBigDecimal(BigDecimal(1), BigDecimal(0))
    c.abs shouldBe BigDecimal(1) +- BigDecimal("0.001")
  }

  "QuaternionDouble" should "handle basic construction and properties" in {
    val q = QuaternionDouble(1, 2, 3, 4)
    q.a shouldBe 1.0
    q.b shouldBe 2.0
    q.c shouldBe 3.0
    q.d shouldBe 4.0

    QuaternionDouble.zero shouldBe QuaternionDouble(0, 0, 0, 0)
    QuaternionDouble.one shouldBe QuaternionDouble(1, 0, 0, 0)
  }

  it should "handle basic arithmetic" in {
    val q1 = QuaternionDouble(1, 0, 0, 0)
    val q2 = QuaternionDouble(0, 1, 0, 0)
    val q3 = QuaternionDouble(1, 2, 3, 4)

    (q1 + q2) shouldBe QuaternionDouble(1, 1, 0, 0)
    (q1 - q2) shouldBe QuaternionDouble(1, -1, 0, 0)
    (q1 * q2) shouldBe QuaternionDouble(0, 1, 0, 0)
    (-q3) shouldBe QuaternionDouble(-1, -2, -3, -4)
    q1.conj shouldBe QuaternionDouble(1, 0, 0, 0)
  }

  "QuaternionRational" should "handle basic operations" in {
    val q1 = QuaternionRational(Rational(1), Rational(0), Rational(0), Rational(0))
    val q2 = QuaternionRational(Rational(0), Rational(1), Rational(0), Rational(0))
    val q3 = QuaternionRational(Rational(1, 2), Rational(1, 3), Rational(1, 4), Rational(1, 5))

    (q1 + q2) shouldBe QuaternionRational(Rational(1), Rational(1), Rational(0), Rational(0))
    (q1 - q2) shouldBe QuaternionRational(Rational(1), Rational(-1), Rational(0), Rational(0))
    (q1 * q2) shouldBe QuaternionRational(Rational(0), Rational(1), Rational(0), Rational(0))
    (-q3) shouldBe QuaternionRational(Rational(-1, 2), Rational(-1, 3), Rational(-1, 4), Rational(-1, 5))
    q1.conj shouldBe QuaternionRational(Rational(1), Rational(0), Rational(0), Rational(0))

    QuaternionRational.i shouldBe QuaternionRational(Rational(0), Rational(1), Rational(0), Rational(0))
    QuaternionRational.j shouldBe QuaternionRational(Rational(0), Rational(0), Rational(1), Rational(0))
    QuaternionRational.k shouldBe QuaternionRational(Rational(0), Rational(0), Rational(0), Rational(1))
  }

  "QuaternionInt" should "handle basic operations" in {
    val q1 = QuaternionInt(1, 0, 0, 0)
    val q2 = QuaternionInt(0, 1, 0, 0)

    (q1 + q2) shouldBe QuaternionInt(1, 1, 0, 0)
    QuaternionInt.i shouldBe QuaternionInt(0, 1, 0, 0)
    QuaternionInt.j shouldBe QuaternionInt(0, 0, 1, 0)
    QuaternionInt.k shouldBe QuaternionInt(0, 0, 0, 1)
  }

  "QuaternionBigInt" should "handle basic operations" in {
    val q1 = QuaternionBigInt(BigInt(1), BigInt(0), BigInt(0), BigInt(0))
    val q2 = QuaternionBigInt(BigInt(0), BigInt(1), BigInt(0), BigInt(0))
    val q3 = QuaternionBigInt(BigInt(1), BigInt(2), BigInt(3), BigInt(4))

    (q1 + q2) shouldBe QuaternionBigInt(BigInt(1), BigInt(1), BigInt(0), BigInt(0))
    (q1 - q2) shouldBe QuaternionBigInt(BigInt(1), BigInt(-1), BigInt(0), BigInt(0))
    (q1 * q2) shouldBe QuaternionBigInt(BigInt(0), BigInt(1), BigInt(0), BigInt(0))
    (-q3) shouldBe QuaternionBigInt(BigInt(-1), BigInt(-2), BigInt(-3), BigInt(-4))
    q1.conj shouldBe QuaternionBigInt(BigInt(1), BigInt(0), BigInt(0), BigInt(0))

    // Test special values
    QuaternionBigInt.zero shouldBe QuaternionBigInt(BigInt(0), BigInt(0), BigInt(0), BigInt(0))
    QuaternionBigInt.one shouldBe QuaternionBigInt(BigInt(1), BigInt(0), BigInt(0), BigInt(0))
    QuaternionBigInt.i shouldBe QuaternionBigInt(BigInt(0), BigInt(1), BigInt(0), BigInt(0))
    QuaternionBigInt.j shouldBe QuaternionBigInt(BigInt(0), BigInt(0), BigInt(1), BigInt(0))
    QuaternionBigInt.k shouldBe QuaternionBigInt(BigInt(0), BigInt(0), BigInt(0), BigInt(1))
  }

  it should "handle large number arithmetic" in {
    val large = BigInt("999999999999999999999999999999")
    val q1    = QuaternionBigInt(large, BigInt(1), BigInt(2), BigInt(3))
    val q2    = QuaternionBigInt(BigInt(1), large, BigInt(4), BigInt(5))

    val result = q1 + q2
    result.a shouldBe (large + 1)
    result.b shouldBe (large + 1)
    result.c shouldBe BigInt(6)
    result.d shouldBe BigInt(8)

    // Test scalar multiplication
    (q1 * 2) shouldBe QuaternionBigInt(large * 2, BigInt(2), BigInt(4), BigInt(6))
  }

  "QuaternionBigDecimal" should "handle basic operations" in {
    import BigDecimalMath.decimal128._

    val q1 = QuaternionBigDecimal(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(0))
    val q2 = QuaternionBigDecimal(BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(0))

    (q1 + q2) shouldBe QuaternionBigDecimal(BigDecimal(1), BigDecimal(1), BigDecimal(0), BigDecimal(0))
    q1.conj shouldBe QuaternionBigDecimal(BigDecimal(1), BigDecimal(0), BigDecimal(0), BigDecimal(0))
  }

  it should "handle high precision values" in {
    import BigDecimalMath.decimal128._

    val precise = BigDecimal("1.234567890123456789012345678901234567890")
    val q       = QuaternionBigDecimal(precise, BigDecimal(0), BigDecimal(0), BigDecimal(0))

    q.a shouldBe precise
    q.doubleValue shouldBe precise.toDouble +- 0.001
  }

  it should "work with quaternion units" in {
    import BigDecimalMath.decimal128._

    val i = QuaternionBigDecimal.i
    val j = QuaternionBigDecimal.j
    val k = QuaternionBigDecimal.k

    i shouldBe QuaternionBigDecimal(BigDecimal(0), BigDecimal(1), BigDecimal(0), BigDecimal(0))
    j shouldBe QuaternionBigDecimal(BigDecimal(0), BigDecimal(0), BigDecimal(1), BigDecimal(0))
    k shouldBe QuaternionBigDecimal(BigDecimal(0), BigDecimal(0), BigDecimal(0), BigDecimal(1))
  }

  "Number conversions" should "work correctly" in {
    val r = Rational(3, 2)
    r.doubleValue shouldBe 1.5 +- 0.001
    r.intValue shouldBe 1

    val c = ComplexDouble(3, 4)
    c.doubleValue shouldBe 5.0 +- 0.001 // absolute value
  }

  "Power operations" should "work for complex numbers" in {
    val c = ComplexDouble(2, 0)
    (c ^ 2) should be(ComplexDouble(4, 0))

    // i^2 = -1
    (ComplexDouble.i ^ 2) should be(ComplexDouble(-1, 0))
  }

  "String representations" should "be reasonable" in {
    Rational(1, 2).toString shouldBe "1/2"
    Rational(2).toString shouldBe "2"
    Rational(0).toString shouldBe "0"

    ComplexDouble(1, 0).toString should include("1")
    ComplexDouble(0, 1).toString should include("i")
  }

  "BigDecimalMath" should "provide basic mathematical constants" in {
    import BigDecimalMath.decimal128._

    // Test that constants are available and reasonable
    val pi = bdmath.Pi.v
    val e  = bdmath.E.v

    pi shouldBe BigDecimal("3.14159") +- BigDecimal("0.01")
    e shouldBe BigDecimal("2.71828") +- BigDecimal("0.01")
  }

  it should "handle basic mathematical functions" in {
    import BigDecimalMath.decimal128._

    val two  = BigDecimal(2)
    val four = BigDecimal(4)

    // sqrt(4) = 2
    BigDecimalMath.sqrt(four) shouldBe two +- BigDecimal("0.001")

    // exp(0) = 1
    BigDecimalMath.exp(BigDecimal(0)) shouldBe BigDecimal(1) +- BigDecimal("0.001")
  }

  "Arbitrary precision" should "maintain precision in calculations" in {
    // Test that we can work with very large numbers
    val hugeBigInt = BigInt("12345678901234567890123456789012345678901234567890")
    val c          = ComplexBigInt(hugeBigInt, BigInt(1))

    c.re shouldBe hugeBigInt
    c.im shouldBe BigInt(1)

    // Test high precision decimals
    import BigDecimalMath.decimal128._
    val preciseDecimal = BigDecimal("1.123456789012345678901234567890123456789")
    val cd             = ComplexBigDecimal(preciseDecimal, BigDecimal(0))

    cd.re.toString should include("1.123456789012345678901234567890123456789")
  }

  "Implicit conversions" should "work correctly" in {
    // Int to various complex types
    val intToComplexDouble: ComplexDouble = 5
    intToComplexDouble shouldBe ComplexDouble(5, 0)

    val intToComplexInt: ComplexInt = 3
    intToComplexInt shouldBe ComplexInt(3, 0)

    // Int to rational
    val intToRational: Rational = 7
    intToRational shouldBe Rational(7)

    // BigInt conversions
    val bigIntToComplexBigInt: ComplexBigInt = BigInt(42)
    bigIntToComplexBigInt shouldBe ComplexBigInt(BigInt(42), BigInt(0))
  }

  "Fractional typeclass instances" should "work with standard library functions" in {
    import ComplexDoubleIsFractional._

    val numbers = List(ComplexDouble(1, 0), ComplexDouble(2, 0), ComplexDouble(3, 0))
    val sum     = numbers.sum
    sum shouldBe ComplexDouble(6, 0)

    // Test with rationals
    val rationals   = List(Rational(1, 2), Rational(1, 3), Rational(1, 6))
    val rationalSum = rationals.sum
    rationalSum shouldBe Rational(1, 1)
  }
}
