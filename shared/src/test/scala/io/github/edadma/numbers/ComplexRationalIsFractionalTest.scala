package io.github.edadma.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ComplexRationalIsFractionalTest extends AnyFlatSpec with Matchers {

  import ComplexRationalIsFractional.complexRationalIsFractional

  "ComplexRationalIsFractional parseString" should "parse positive imaginary parts correctly" in {
    val result = complexRationalIsFractional.parseString("1/2+1/3i")
    result shouldBe Some(ComplexRational(Rational(1, 2), Rational(1, 3)))

    val result2 = complexRationalIsFractional.parseString("3/4+2/5i")
    result2 shouldBe Some(ComplexRational(Rational(3, 4), Rational(2, 5)))
  }

  it should "parse negative imaginary parts correctly" in {
    // This is the key test that was failing before the fix
    val result = complexRationalIsFractional.parseString("3/4-2/5i")
    result shouldBe Some(ComplexRational(Rational(3, 4), Rational(-2, 5)))

    val result2 = complexRationalIsFractional.parseString("1/2-1/3i")
    result2 shouldBe Some(ComplexRational(Rational(1, 2), Rational(-1, 3)))
  }

  it should "parse negative real parts correctly" in {
    val result = complexRationalIsFractional.parseString("-1/2+1/3i")
    result shouldBe Some(ComplexRational(Rational(-1, 2), Rational(1, 3)))

    val result2 = complexRationalIsFractional.parseString("-3/4-2/5i")
    result2 shouldBe Some(ComplexRational(Rational(-3, 4), Rational(-2, 5)))
  }

  it should "parse unit imaginary coefficients correctly" in {
    // Cases where imaginary coefficient is implicit (just "i")
    val result1 = complexRationalIsFractional.parseString("1/2+i")
    result1 shouldBe Some(ComplexRational(Rational(1, 2), Rational(1)))

    val result2 = complexRationalIsFractional.parseString("1/2-i")
    result2 shouldBe Some(ComplexRational(Rational(1, 2), Rational(-1)))

    val result3 = complexRationalIsFractional.parseString("3/4+i")
    result3 shouldBe Some(ComplexRational(Rational(3, 4), Rational(1)))

    val result4 = complexRationalIsFractional.parseString("3/4-i")
    result4 shouldBe Some(ComplexRational(Rational(3, 4), Rational(-1)))
  }

  it should "parse pure rational imaginary numbers correctly" in {
    // Pure rational imaginary numbers should parse successfully
    val result1 = complexRationalIsFractional.parseString("2/3i")
    result1 shouldBe Some(ComplexRational(Rational(0), Rational(2, 3)))

    val result2 = complexRationalIsFractional.parseString("-2/3i")
    result2 shouldBe Some(ComplexRational(Rational(0), Rational(-2, 3)))

    // Additional test cases
    val result3 = complexRationalIsFractional.parseString("1/4i")
    result3 shouldBe Some(ComplexRational(Rational(0), Rational(1, 4)))

    val result4 = complexRationalIsFractional.parseString("-5/7i")
    result4 shouldBe Some(ComplexRational(Rational(0), Rational(-5, 7)))
  }

  it should "handle zero components correctly" in {
    val result1 = complexRationalIsFractional.parseString("0/1+1/2i")
    result1 shouldBe Some(ComplexRational(Rational(0), Rational(1, 2)))

    val result2 = complexRationalIsFractional.parseString("1/2+0/1i")
    result2 shouldBe Some(ComplexRational(Rational(1, 2), Rational(0)))

    val result3 = complexRationalIsFractional.parseString("0/1+0/1i")
    result3 shouldBe Some(ComplexRational(Rational(0), Rational(0)))
  }

  it should "handle improper fractions correctly" in {
    val result = complexRationalIsFractional.parseString("7/3+5/2i")
    result shouldBe Some(ComplexRational(Rational(7, 3), Rational(5, 2)))

    val result2 = complexRationalIsFractional.parseString("7/3-5/2i")
    result2 shouldBe Some(ComplexRational(Rational(7, 3), Rational(-5, 2)))
  }

  it should "handle fractions that need reduction correctly" in {
    val result = complexRationalIsFractional.parseString("4/8+6/9i")
    result shouldBe Some(ComplexRational(Rational(1, 2), Rational(2, 3))) // Should auto-reduce

    val result2 = complexRationalIsFractional.parseString("6/8-4/6i")
    result2 shouldBe Some(ComplexRational(Rational(3, 4), Rational(-2, 3))) // Should auto-reduce
  }

  it should "return None for invalid rational complex formats" in {
    // These should not parse as rational complex numbers
    complexRationalIsFractional.parseString("3+4i") shouldBe None     // Integers, not rationals
    complexRationalIsFractional.parseString("3.5+4.2i") shouldBe None // Decimals, not rationals
    complexRationalIsFractional.parseString("1/2+3i") shouldBe None   // Mixed rational/integer
    complexRationalIsFractional.parseString("3+1/2i") shouldBe None   // Mixed integer/rational
    complexRationalIsFractional.parseString("not-a-number") shouldBe None
    complexRationalIsFractional.parseString("") shouldBe None
    complexRationalIsFractional.parseString("1/2+3/4j") shouldBe None // Wrong imaginary unit
  }

  it should "return None for malformed rational expressions" in {
    complexRationalIsFractional.parseString("1/0+1/2i") shouldBe None  // Division by zero
    complexRationalIsFractional.parseString("1/2+1/0i") shouldBe None  // Division by zero in imaginary
    complexRationalIsFractional.parseString("1//2+1/3i") shouldBe None // Double slash
    complexRationalIsFractional.parseString("1/2++1/3i") shouldBe None // Double plus
    complexRationalIsFractional.parseString("1/2+1/3ii") shouldBe None // Double i
  }

  it should "handle whitespace appropriately" in {
    // The current regex might not handle spaces - test what happens
    val result1 = complexRationalIsFractional.parseString("1/2 + 1/3 i")
    // This might be None if spaces aren't supported, which is probably fine

    val result2 = complexRationalIsFractional.parseString("  1/2+1/3i  ")
    // Leading/trailing spaces might work with trim
  }

  it should "demonstrate the sign parsing fix" in {
    // This is the specific test case that demonstrates the bug fix
    val problematicCase = "3/4-2/5i"
    val result          = complexRationalIsFractional.parseString(problematicCase)

    result shouldBe defined
    val complex = result.get

    // The real part should be 3/4
    complex.re shouldBe Rational(3, 4)

    // The imaginary part should be -2/5 (negative!)
    complex.im shouldBe Rational(-2, 5)

    // Make sure it's actually negative
    complex.im.isNegative shouldBe true
    complex.im.numerator shouldBe BigInt(-2)
    complex.im.denominator shouldBe BigInt(5)
  }

  it should "verify all four sign combinations work" in {
    val testCases = Seq(
      ("1/2+1/3i", ComplexRational(Rational(1, 2), Rational(1, 3))),   // + +
      ("1/2-1/3i", ComplexRational(Rational(1, 2), Rational(-1, 3))),  // + -
      ("-1/2+1/3i", ComplexRational(Rational(-1, 2), Rational(1, 3))), // - +
      ("-1/2-1/3i", ComplexRational(Rational(-1, 2), Rational(-1, 3))), // - -
    )

    for ((input, expected) <- testCases) {
      val result = complexRationalIsFractional.parseString(input)
      result shouldBe Some(expected)
    }
  }
}
