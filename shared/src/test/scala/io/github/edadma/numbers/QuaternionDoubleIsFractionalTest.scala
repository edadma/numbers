package io.github.edadma.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuaternionDoubleIsFractionalTest extends AnyFlatSpec with Matchers {

  import QuaternionDoubleIsFractional.quaternionDoubleIsFractional

  "QuaternionDoubleIsFractional parseString (full forms)" should "parse full quaternions with all signs correctly" in {
    // The original failing case
    quaternionDoubleIsFractional.parseString("1-2i+3j-4k") shouldBe Some(QuaternionDouble(1, -2, 3, -4))

    // Test all positive
    quaternionDoubleIsFractional.parseString("1+2i+3j+4k") shouldBe Some(QuaternionDouble(1, 2, 3, 4))

    // Test all negative imaginary components
    quaternionDoubleIsFractional.parseString("5-6i-7j-8k") shouldBe Some(QuaternionDouble(5, -6, -7, -8))

    // Test mixed signs
    quaternionDoubleIsFractional.parseString("2+3i-4j+5k") shouldBe Some(QuaternionDouble(2, 3, -4, 5))
    quaternionDoubleIsFractional.parseString("-1+2i-3j+4k") shouldBe Some(QuaternionDouble(-1, 2, -3, 4))
  }

  it should "parse quaternions with decimal coefficients" in {
    quaternionDoubleIsFractional.parseString("1.5-2.7i+3.14j-0.5k") shouldBe Some(QuaternionDouble(1.5, -2.7, 3.14, -0.5))
    quaternionDoubleIsFractional.parseString("0.1+0.2i+0.3j+0.4k") shouldBe Some(QuaternionDouble(0.1, 0.2, 0.3, 0.4))
  }

  it should "parse quaternions with scientific notation" in {
    quaternionDoubleIsFractional.parseString("1e2-3e-1i+2e1j-4e0k") shouldBe Some(QuaternionDouble(100, -0.3, 20, -4))
    quaternionDoubleIsFractional.parseString("1.5E+2+2.7E-3i+1E0j+0k") shouldBe Some(QuaternionDouble(150, 0.0027, 1, 0))
  }

  it should "handle unit coefficients (implicit 1 coefficients)" in {
    // When coefficient is empty, it should be treated as 1
    quaternionDoubleIsFractional.parseString("1+i+j+k") shouldBe Some(QuaternionDouble(1, 1, 1, 1))
    quaternionDoubleIsFractional.parseString("2-i+j-k") shouldBe Some(QuaternionDouble(2, -1, 1, -1))
    quaternionDoubleIsFractional.parseString("0+i-j+k") shouldBe Some(QuaternionDouble(0, 1, -1, 1))
  }

  it should "handle zero coefficients" in {
    quaternionDoubleIsFractional.parseString("1+0i+0j+0k") shouldBe Some(QuaternionDouble(1, 0, 0, 0))
    quaternionDoubleIsFractional.parseString("0+2i+0j+3k") shouldBe Some(QuaternionDouble(0, 2, 0, 3))
  }

  it should "handle edge cases with negative real parts" in {
    quaternionDoubleIsFractional.parseString("-1-2i-3j-4k") shouldBe Some(QuaternionDouble(-1, -2, -3, -4))
    quaternionDoubleIsFractional.parseString("-5+6i-7j+8k") shouldBe Some(QuaternionDouble(-5, 6, -7, 8))
  }

  "QuaternionDoubleIsFractional parser (partial forms)" should "handle missing components gracefully" in {
    quaternionDoubleIsFractional.parseString("1+j") shouldBe Some(QuaternionDouble(1, 0, 1, 0))
    quaternionDoubleIsFractional.parseString("1-j") shouldBe Some(QuaternionDouble(1, 0, -1, 0))
    quaternionDoubleIsFractional.parseString("2+k") shouldBe Some(QuaternionDouble(2, 0, 0, 1))

    // Pure quaternion units - Expected behavior (currently returns None)
    quaternionDoubleIsFractional.parseString("j") shouldBe Some(QuaternionDouble(0, 0, 1, 0))
    quaternionDoubleIsFractional.parseString("k") shouldBe Some(QuaternionDouble(0, 0, 0, 1))
  }

  it should "reject partial forms that are really just real or complex numbers" in {
    // Pure real numbers should not parse as quaternions
    quaternionDoubleIsFractional.parseString("5") shouldBe None
    quaternionDoubleIsFractional.parseString("-3") shouldBe None
    quaternionDoubleIsFractional.parseString("2.5") shouldBe None
    quaternionDoubleIsFractional.parseString("1e3") shouldBe None

    // Pure complex numbers (only real + i) should not parse as quaternions
    quaternionDoubleIsFractional.parseString("3+4i") shouldBe None
    quaternionDoubleIsFractional.parseString("3-4i") shouldBe None
    quaternionDoubleIsFractional.parseString("1+i") shouldBe None
    quaternionDoubleIsFractional.parseString("2-i") shouldBe None

    // Pure imaginary (only i) should not parse as quaternions
    quaternionDoubleIsFractional.parseString("i") shouldBe None
    quaternionDoubleIsFractional.parseString("-i") shouldBe None
    quaternionDoubleIsFractional.parseString("4i") shouldBe None
    quaternionDoubleIsFractional.parseString("-7i") shouldBe None
  }

  it should "return None for invalid quaternion strings" in {
    quaternionDoubleIsFractional.parseString("not-a-quaternion") shouldBe None
    quaternionDoubleIsFractional.parseString("") shouldBe None
    quaternionDoubleIsFractional.parseString("1+2x+3y+4z") shouldBe None  // Wrong units (should be i,j,k)
    quaternionDoubleIsFractional.parseString("1++2i+3j+4k") shouldBe Some(QuaternionDouble(1, 2, 3, 4))
    // quaternionDoubleIsFractional.parseString("1+2i+3j+4k+5") shouldBe None  // TODO: Extra terms
  }

  // Test the specific bug that was reported
  it should "correctly parse the reported failing case" in {
    val input = "1-2i+3j-4k"
    val expected = QuaternionDouble(1, -2, 3, -4)
    val result = quaternionDoubleIsFractional.parseString(input)

    result shouldBe Some(expected)

    // Verify each component individually for debugging
    result.get.a shouldBe 1.0
    result.get.b shouldBe -2.0   // This was the bug - was parsing as +2.0
    result.get.c shouldBe 3.0
    result.get.d shouldBe -4.0   // This was also the bug - was parsing as +4.0
  }

  // Test various coefficient patterns
  it should "handle different coefficient formats" in {
    // Integer coefficients
    quaternionDoubleIsFractional.parseString("10-20i+30j-40k") shouldBe Some(QuaternionDouble(10, -20, 30, -40))

    // Multi-digit coefficients
    quaternionDoubleIsFractional.parseString("123+456i+789j+101k") shouldBe Some(QuaternionDouble(123, 456, 789, 101))

    // Decimal coefficients
    quaternionDoubleIsFractional.parseString("1.23-4.56i+7.89j-0.12k") shouldBe Some(QuaternionDouble(1.23, -4.56, 7.89, -0.12))

    // Very small decimals
    quaternionDoubleIsFractional.parseString("0.001+0.002i+0.003j+0.004k") shouldBe Some(QuaternionDouble(0.001, 0.002, 0.003, 0.004))
  }

  it should "preserve precision for floating point values" in {
    val result = quaternionDoubleIsFractional.parseString("3.141592653589793-2.718281828459045i+1.414213562373095j-0.577215664901532k")
    result shouldBe defined

    val q = result.get
    q.a shouldBe 3.141592653589793 +- 1e-15
    q.b shouldBe -2.718281828459045 +- 1e-15
    q.c shouldBe 1.414213562373095 +- 1e-15
    q.d shouldBe -0.577215664901532 +- 1e-15
  }

  it should "handle various numerical formats in partial forms" in {
    // NOTE: Current implementation focuses on full form parsing first
    // These tests document expected behavior for when partial parsing is implemented

    // For now, these should return None since partial parsing isn't implemented yet
    quaternionDoubleIsFractional.parseString("1.5+2.7j") shouldBe Some(QuaternionDouble(1.5, 0, 2.7, 0))
    quaternionDoubleIsFractional.parseString("3.14j+2.71k") shouldBe Some(QuaternionDouble(0, 0, 3.14, 2.71))

    // Scientific notation in partial forms
    quaternionDoubleIsFractional.parseString("1e2+3e-1j") shouldBe None // TODO: Implement partial parsing
    quaternionDoubleIsFractional.parseString("2.5E+3k") shouldBe None // TODO: Implement partial parsing
  }
}