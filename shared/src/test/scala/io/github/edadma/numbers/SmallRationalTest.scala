package io.github.edadma.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmallRationalTest extends AnyFlatSpec with Matchers {

  "SmallRational" should "handle basic arithmetic" in {
    val r1 = SmallRational(1L, 2L)
    val r2 = SmallRational(1L, 3L)

    (r1 + r2) shouldBe SmallRational(5L, 6L)
    (r1 - r2) shouldBe SmallRational(1L, 6L)
    (r1 * r2) shouldBe SmallRational(1L, 6L)
    (r1 / r2) shouldBe SmallRational(3L, 2L)
  }

  it should "handle reduction automatically" in {
    SmallRational(4L, 8L) shouldBe SmallRational(1L, 2L)
    SmallRational(6L, 9L) shouldBe SmallRational(2L, 3L)
    SmallRational(-4L, 8L) shouldBe SmallRational(-1L, 2L)
    SmallRational(4L, -8L) shouldBe SmallRational(-1L, 2L)
  }

  it should "handle special values" in {
    SmallRational.ZERO shouldBe SmallRational(0L)
    SmallRational.ONE shouldBe SmallRational(1L)
    SmallRational.HALF shouldBe SmallRational(1L, 2L)

    SmallRational.third shouldBe SmallRational(1L, 3L)
    SmallRational.quarter shouldBe SmallRational(1L, 4L)
    SmallRational.fifth shouldBe SmallRational(1L, 5L)
  }

  it should "handle power operations" in {
    SmallRational(2L, 3L) ^ 2 shouldBe SmallRational(4L, 9L)
    SmallRational(2L) ^ 3 shouldBe SmallRational(8L)
    SmallRational(2L, 3L) ^ 0 shouldBe SmallRational.ONE
    SmallRational(3L, 2L) ^ -1 shouldBe SmallRational(2L, 3L)
  }

  it should "handle negation and absolute value" in {
    val r = SmallRational(-3L, 4L)
    (-r) shouldBe SmallRational(3L, 4L)
    r.abs shouldBe SmallRational(3L, 4L)

    val positive = SmallRational(3L, 4L)
    positive.abs shouldBe positive
  }

  it should "handle inverse operations" in {
    val r = SmallRational(3L, 4L)
    r.inv shouldBe SmallRational(4L, 3L)

    val whole = SmallRational(5L)
    whole.inv shouldBe SmallRational(1L, 5L)
  }

  it should "handle floor and ceiling operations" in {
    val r = SmallRational(7L, 3L) // 2.333...
    r.floor shouldBe SmallRational(2L)
    r.ceil shouldBe SmallRational(3L)

    val negative = SmallRational(-7L, 3L) // -2.333...
    negative.floor shouldBe SmallRational(-3L)
    negative.ceil shouldBe SmallRational(-2L)

    val whole = SmallRational(6L, 3L) // 2.0
    whole.floor shouldBe SmallRational(2L)
    whole.ceil shouldBe SmallRational(2L)
  }

  it should "handle comparison operations" in {
    val r1 = SmallRational(1L, 2L)
    val r2 = SmallRational(2L, 3L)
    val r3 = SmallRational(1L, 2L)

    r1 should be < r2
    r2 should be > r1
    r1 shouldBe r3
    r1.compare(r2) should be < 0
    r2.compare(r1) should be > 0
    r1.compare(r3) shouldBe 0
  }

  it should "handle mixed number conversion" in {
    val r                   = SmallRational(7L, 3L) // 2 + 1/3
    val (whole, fractional) = r.toMixedNumber
    whole shouldBe 2L
    fractional shouldBe SmallRational(1L, 3L)

    val negative                  = SmallRational(-7L, 3L) // -2 - 1/3
    val (negWhole, negFractional) = negative.toMixedNumber
    negWhole shouldBe -2L
    negFractional shouldBe SmallRational(-1L, 3L)
  }

  it should "handle mediant operation" in {
    val r1 = SmallRational(1L, 2L)
    val r2 = SmallRational(2L, 3L)
    r1.mediant(r2) shouldBe SmallRational(3L, 5L)
  }

  it should "handle string parsing" in {
    SmallRational("1/2") shouldBe SmallRational(1L, 2L)
    SmallRational("3/4") shouldBe SmallRational(3L, 4L)
    SmallRational("-5/6") shouldBe SmallRational(-5L, 6L)
    SmallRational("  7 / 8  ") shouldBe SmallRational(7L, 8L)
  }

  it should "handle conversion to other number types" in {
    val r = SmallRational(3L, 2L)
    r.doubleValue shouldBe 1.5 +- 0.001
    r.floatValue shouldBe 1.5f +- 0.001f
    r.intValue shouldBe 1
    r.longValue shouldBe 1L

    val whole = SmallRational(5L)
    whole.intValue shouldBe 5
    whole.longValue shouldBe 5L
  }

  it should "handle conversions to Rational" in {
    val sr = SmallRational(3L, 4L)
    val r  = sr.toRational
    r shouldBe Rational(3, 4)
    r.numerator shouldBe BigInt(3)
    r.denominator shouldBe BigInt(4)
  }

  it should "handle equality with different types" in {
    val sr = SmallRational(6L, 3L) // 2
    sr shouldBe 2
    sr shouldBe 2L
    sr shouldBe SmallRational(2L, 1L)

    val sr2 = SmallRational(1L, 2L)
    sr2 should not be 1
    sr2 should not be SmallRational(1L, 3L)
  }

  it should "handle properties correctly" in {
    val zero = SmallRational.ZERO
    zero.isZero shouldBe true
    zero.nonZero shouldBe false
    zero.isWhole shouldBe true
    zero.signum shouldBe 0
    zero.isPositive shouldBe false
    zero.isNegative shouldBe false

    val positive = SmallRational(3L, 4L)
    positive.isZero shouldBe false
    positive.nonZero shouldBe true
    positive.isWhole shouldBe false
    positive.signum shouldBe 1
    positive.isPositive shouldBe true
    positive.isNegative shouldBe false

    val negative = SmallRational(-3L, 4L)
    negative.signum shouldBe -1
    negative.isPositive shouldBe false
    negative.isNegative shouldBe true

    val whole = SmallRational(5L)
    whole.isWhole shouldBe true
  }

  it should "handle arithmetic with integers and longs" in {
    val r = SmallRational(1L, 2L)

    (r + 1) shouldBe SmallRational(3L, 2L)
    (r + 1L) shouldBe SmallRational(3L, 2L)
    (r * 2) shouldBe SmallRational(1L, 1L)
    (r * 2L) shouldBe SmallRational(1L, 1L)
    (r - 1) shouldBe SmallRational(-1L, 2L)
    (r - 1L) shouldBe SmallRational(-1L, 2L)
    (r / 2) shouldBe SmallRational(1L, 4L)
    (r / 2L) shouldBe SmallRational(1L, 4L)
  }

  it should "detect overflow conditions" in {
    // Test cases that should cause overflow using overflow-checking methods
    val large = SmallRational(Long.MaxValue, 1L)

    a[ArithmeticException] should be thrownBy {
      large.multiplyExact(SmallRational(2L))
    }

    a[ArithmeticException] should be thrownBy {
      large.addExact(large)
    }

    // Test subtractExact overflow
    val negLarge = SmallRational(Long.MinValue, 1L)
    a[ArithmeticException] should be thrownBy {
      negLarge.subtractExact(SmallRational(1L))
    }

    // Test divideExact overflow
    a[ArithmeticException] should be thrownBy {
      large.divideExact(SmallRational(1L, Long.MaxValue))
    }
  }

  it should "handle edge cases in overflow checking" in {
    // Test Long.MinValue special cases
    val minVal = SmallRational(Long.MinValue, 1L)

    a[ArithmeticException] should be thrownBy {
      minVal.multiplyExact(SmallRational(-1L))
    }

    // Test that safe operations work for edge cases
    val result1 = minVal.multiplyExact(SmallRational(0L)) // 0 * anything = 0, should not overflow
    result1 shouldBe SmallRational.ZERO

    val result2 = minVal.multiplyExact(SmallRational(1L)) // 1 * anything = anything, should not overflow
    result2 shouldBe minVal

    // Test division by powers of 2 near overflow boundaries
    val almostOverflow = SmallRational(Long.MaxValue / 2, 1L)
    val doubled        = almostOverflow.multiplyExact(SmallRational(2L)) // Should not overflow
    doubled shouldBe SmallRational(Long.MaxValue - 1, 1L)

    a[ArithmeticException] should be thrownBy {
      doubled.multiplyExact(SmallRational(2L)) // This should overflow
    }
  }

  it should "provide safe operations for overflow handling" in {
    val large = SmallRational(Long.MaxValue, 1L)

    SmallRational.safeMultiply(large, SmallRational(2L)) shouldBe None
    SmallRational.safeAdd(large, large) shouldBe None
    SmallRational.safeSubtract(SmallRational(Long.MinValue, 1L), SmallRational(1L)) shouldBe None
    SmallRational.safeDivide(large, SmallRational(1L, Long.MaxValue)) shouldBe None

    val small1 = SmallRational(1L, 2L)
    val small2 = SmallRational(1L, 3L)

    SmallRational.safeAdd(small1, small2) shouldBe Some(SmallRational(5L, 6L))
    SmallRational.safeMultiply(small1, small2) shouldBe Some(SmallRational(1L, 6L))
    SmallRational.safeSubtract(small1, small2) shouldBe Some(SmallRational(1L, 6L))
    SmallRational.safeDivide(small1, small2) shouldBe Some(SmallRational(3L, 2L))
  }

  it should "test overflow checking functions comprehensively" in {
    // Test multiply overflow boundary conditions
    val halfMax = SmallRational(Long.MaxValue / 2, 1L)

    // This should work (just under the boundary)
    val almostMax = halfMax.multiplyExact(SmallRational(2L))
    almostMax.numerator shouldBe (Long.MaxValue - 1)

    // Test negative multiplication overflow
    val negHalfMax = SmallRational(Long.MinValue / 2, 1L)
    val almostMin  = negHalfMax.multiplyExact(SmallRational(2L))
    almostMin.numerator shouldBe Long.MinValue

    // Test addition overflow boundaries
    val result1 = SmallRational(Long.MaxValue - 1, 1L).addExact(SmallRational(1L))
    result1 shouldBe SmallRational(Long.MaxValue, 1L)

    a[ArithmeticException] should be thrownBy {
      SmallRational(Long.MaxValue, 1L).addExact(SmallRational(1L))
    }

    // Test subtraction overflow boundaries
    val result2 = SmallRational(Long.MinValue + 1, 1L).subtractExact(SmallRational(1L))
    result2 shouldBe SmallRational(Long.MinValue, 1L)

    a[ArithmeticException] should be thrownBy {
      SmallRational(Long.MinValue, 1L).subtractExact(SmallRational(1L))
    }
  }

  it should "verify that regular operations don't perform overflow checking" in {
    // These should complete without throwing, even though they overflow
    val large = SmallRational(Long.MaxValue, 1L)

    // Regular operations should not throw ArithmeticException (they may overflow silently)
    noException should be thrownBy {
      large + large // This will overflow but shouldn't throw
    }

    noException should be thrownBy {
      large * SmallRational(2L) // This will overflow but shouldn't throw
    }

    // The results will be wrong due to overflow, but no exception should be thrown
    val overflowResult = large + large
    // Just verify it's not the mathematically correct result (which would require BigInt)
    overflowResult should not be SmallRational(Long.MaxValue, 1L) // It's definitely not the original value

    // Test that the overflowed result is some actual SmallRational (not null or throwing)
    overflowResult.numerator should be < Long.MaxValue // Due to overflow wraparound
  }

  it should "allow fast arithmetic without overflow checking" in {
    // Normal operations are fast and don't check overflow
    val a = SmallRational(1000000L, 1L)
    val b = SmallRational(2000000L, 1L)

    val sum     = a + b // Fast, no overflow checking
    val product = a * b // Fast, no overflow checking

    sum shouldBe SmallRational(3000000L, 1L)
    product shouldBe SmallRational(2000000000000L, 1L)
  }

  it should "work with implicit conversions" in {
    import SmallRational.{intdiv2smallRational, longdiv2smallRational}

    val r: SmallRational = 5 // Int to SmallRational
    r shouldBe SmallRational(5L)

    val r2: SmallRational = 7L // Long to SmallRational
    r2 shouldBe SmallRational(7L)

    // Test the division operator
    val fraction = 3 \ 4 // Using IntDiv
    fraction shouldBe SmallRational(3L, 4L)

    val fraction2 = 5L \ 6L // Using LongDiv
    fraction2 shouldBe SmallRational(5L, 6L)
  }

  "ComplexSmallRational" should "handle basic complex arithmetic" in {
    val c1 = ComplexSmallRational(SmallRational(1L, 2L), SmallRational(1L, 3L))
    val c2 = ComplexSmallRational(SmallRational(1L, 4L), SmallRational(1L, 6L))

    (c1 + c2) shouldBe ComplexSmallRational(SmallRational(3L, 4L), SmallRational(1L, 2L))
    c1.conj shouldBe ComplexSmallRational(SmallRational(1L, 2L), SmallRational(-1L, 3L))
  }

  it should "handle special values" in {
    ComplexSmallRational.zero shouldBe ComplexSmallRational(SmallRational.ZERO, SmallRational.ZERO)
    ComplexSmallRational.one shouldBe ComplexSmallRational(SmallRational.ONE, SmallRational.ZERO)
    ComplexSmallRational.i shouldBe ComplexSmallRational(SmallRational.ZERO, SmallRational.ONE)
  }

  it should "handle implicit conversions" in {
    val c: ComplexSmallRational = SmallRational(1L, 2L)
    c shouldBe ComplexSmallRational(SmallRational(1L, 2L), SmallRational.ZERO)

    val c2: ComplexSmallRational = 3
    c2 shouldBe ComplexSmallRational(SmallRational(3L), SmallRational.ZERO)
  }

  "SmallRational Fractional instance" should "work with standard library functions" in {
    import SmallRational.smallRationalIsFractional

    val rationals = List(SmallRational(1L, 2L), SmallRational(1L, 3L), SmallRational(1L, 6L))
    val sum       = rationals.sum
    sum shouldBe SmallRational(1L, 1L) // 1/2 + 1/3 + 1/6 = 3/6 + 2/6 + 1/6 = 6/6 = 1

    val product = List(SmallRational(1L, 2L), SmallRational(2L, 3L)).product
    product shouldBe SmallRational(1L, 3L) // (1/2) * (2/3) = 2/6 = 1/3
  }

  "Performance comparison" should "demonstrate benefits over BigInt-based Rational" in {
    // This is more of a conceptual test - in real usage, SmallRational
    // should be significantly faster for small numbers

    val sr1 = SmallRational(123L, 456L)
    val sr2 = SmallRational(789L, 234L)

    val r1 = Rational(123, 456)
    val r2 = Rational(789, 234)

    // Both should give the same mathematical result
    val srResult = sr1 + sr2
    val rResult  = r1 + r2

    srResult.toRational shouldBe rResult
  }

  "String representation" should "be clean and readable" in {
    SmallRational.ZERO.toString shouldBe "0"
    SmallRational.ONE.toString shouldBe "1"
    SmallRational(1L, 2L).toString shouldBe "1/2"
    SmallRational(-3L, 4L).toString shouldBe "-3/4"
    SmallRational(6L, 3L).toString shouldBe "2" // Whole number
  }

  "Edge cases" should "be handled correctly" in {
    // Zero denominator should throw
    a[IllegalArgumentException] should be thrownBy {
      SmallRational(1L, 0L)
    }

    // Division by zero should throw
    a[RuntimeException] should be thrownBy {
      SmallRational.ONE / SmallRational.ZERO
    }

    // Zero inverse should throw
    a[RuntimeException] should be thrownBy {
      SmallRational.ZERO.inv
    }

    // 0^0 should throw
    a[RuntimeException] should be thrownBy {
      SmallRational.ZERO ^ 0
    }
  }
}
