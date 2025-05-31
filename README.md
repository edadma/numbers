# numbers

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/numbers_sjs1_3)
![GitHub](https://img.shields.io/github/license/edadma/numbers)
[![Scala 3](https://img.shields.io/badge/Scala-3.7.0-red.svg)](https://scala-lang.org/)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.19.0-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-5.7-blue.svg)
[![License: ISC](https://img.shields.io/badge/License-ISC-blue.svg)](https://opensource.org/licenses/ISC)

A comprehensive Scala 3 library providing high-precision mathematical number types including rational numbers, complex numbers, and quaternions with support for arbitrary precision arithmetic.

## Overview

The `numbers` library extends Scala's numeric ecosystem with mathematical types that maintain precision and provide exact arithmetic operations. It supports cross-compilation for JVM, JavaScript, and Native platforms, making it suitable for scientific computing, financial calculations, and any application requiring precise mathematical operations.

Key features include:
- **Rational Numbers**: Exact fraction arithmetic with automatic reduction
- **Complex Numbers**: Full complex arithmetic with multiple precision levels
- **Quaternions**: 4D number system for 3D rotations and advanced mathematics
- **Arbitrary Precision**: BigDecimal-based types for unlimited precision
- **Functional Integration**: Scala standard library integration via Fractional typeclasses
- **Cross-Platform**: JVM, JavaScript, and Native support

## Installation

Add the following to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "numbers" % "0.0.1"
```

## Basic Usage

### Rational Numbers

```scala
import io.github.edadma.numbers._

// Create rational numbers
val half = Rational(1, 2)
val third = Rational(1, 3)

// Exact arithmetic
val sum = half + third        // 5/6
val product = half * third    // 1/6
val power = half ^ 3          // 1/8

// Automatic reduction
val reduced = Rational(4, 8)  // 1/2

// Convert to decimal
val decimal = half.doubleValue // 0.5
```

### Complex Numbers

```scala
import io.github.edadma.numbers._

// Double precision complex numbers
val z1 = ComplexDouble(3, 4)
val z2 = ComplexDouble(1, 2)

val sum = z1 + z2             // 4+6i
val product = z1 * z2         // -5+10i
val conjugate = z1.conj       // 3-4i
val magnitude = z1.abs        // 5.0

// Mathematical functions
val exponential = z1.exp
val logarithm = z1.ln
val sine = z1.sin

// Rational coefficient complex numbers
val zr = ComplexRational(Rational(1,2), Rational(1,3))

// Arbitrary precision complex numbers
import BigDecimalMath.decimal128._
val zbd = ComplexBigDecimal(BigDecimal("1.23456789"), BigDecimal("2.98765432"))
```

### Quaternions

```scala
import io.github.edadma.numbers._

// Create quaternions (a + bi + cj + dk)
val q1 = QuaternionDouble(1, 2, 3, 4)
val q2 = QuaternionDouble(2, 1, 0, 1)

// Quaternion arithmetic
val sum = q1 + q2
val product = q1 * q2         // Non-commutative multiplication
val conjugate = q1.conj       // 1-2i-3j-4k
val norm = q1.abs

// Unit quaternions
val i = QuaternionDouble.i    // 0+1i+0j+0k
val j = QuaternionDouble.j    // 0+0i+1j+0k  
val k = QuaternionDouble.k    // 0+0i+0j+1k

// Rational quaternions for exact arithmetic
val qr = QuaternionRational(Rational(1,2), Rational(1,3), Rational(1,4), Rational(1,5))
```

### High-Precision Mathematics

```scala
import io.github.edadma.numbers._
import BigDecimalMath.decimal128._

// High-precision mathematical constants
val pi = bdmath.Pi.v
val e = bdmath.E.v

// High-precision functions
val sqrt2 = BigDecimalMath.sqrt(BigDecimal(2))
val ln10 = BigDecimalMath.ln(BigDecimal(10))
val sin30 = BigDecimalMath.sin(pi / 6)

// Complex arithmetic with arbitrary precision
val z = ComplexBigDecimal(pi, e)
val result = z.exp
```

## API Reference

### Number Types

#### Rational
- `Rational(numerator, denominator)`: Create a rational number
- `+`, `-`, `*`, `/`: Basic arithmetic operations
- `^(Int)`, `^(BigInt)`: Power operations
- `inv`: Multiplicative inverse
- `abs`: Absolute value
- `floor`, `ceil`: Rounding operations

#### Complex Numbers
Available in multiple precision levels:
- `ComplexInt`: Integer coefficients
- `ComplexDouble`: Double precision
- `ComplexRational`: Rational coefficients
- `ComplexBigInt`: BigInt coefficients
- `ComplexBigDecimal`: Arbitrary precision

Operations:
- `+`, `-`, `*`, `/`: Complex arithmetic
- `conj`: Complex conjugate
- `abs`: Magnitude
- `arg`: Argument (phase angle)
- `exp`, `ln`, `sqrt`: Exponential functions
- `sin`, `cos`, `tan`: Trigonometric functions
- `sinh`, `cosh`, `tanh`: Hyperbolic functions

#### Quaternions
Available in multiple precision levels:
- `QuaternionInt`: Integer coefficients
- `QuaternionDouble`: Double precision
- `QuaternionRational`: Rational coefficients
- `QuaternionBigInt`: BigInt coefficients
- `QuaternionBigDecimal`: Arbitrary precision

Operations:
- `+`, `-`, `*`: Quaternion arithmetic (multiplication is non-commutative)
- `conj`: Quaternion conjugate
- `abs`: Magnitude
- `inverse`: Multiplicative inverse
- `exp`, `ln`: Exponential functions

### Precision Control

```scala
import BigDecimalMath._

// Use different precision contexts
object decimal64 {
  implicit val bdmath = new BigDecimalMath(16) // 16 decimal places
}

object decimal256 {
  implicit val bdmath = new BigDecimalMath(77) // 77 decimal places  
}
```

### Integration with Scala Collections

The library provides `Fractional` typeclass instances for seamless integration:

```scala
import io.github.edadma.numbers._
import ComplexDoubleIsFractional._

val complexNumbers = List(ComplexDouble(1,0), ComplexDouble(2,0), ComplexDouble(3,0))
val sum = complexNumbers.sum              // ComplexDouble(6,0)
val product = complexNumbers.product      // ComplexDouble(6,0)

val rationals = List(Rational(1,2), Rational(1,3), Rational(1,6))
val rationalSum = rationals.sum           // Rational(1,1)
```

## Examples

### Scientific Computing
```scala
import io.github.edadma.numbers._

// Mandelbrot set iteration
def mandelbrot(c: ComplexDouble, maxIter: Int = 100): Int = {
  var z = ComplexDouble.zero
  var n = 0
  while (z.abs <= 2.0 && n < maxIter) {
    z = z * z + c
    n += 1
  }
  n
}

val point = ComplexDouble(-0.75, 0.1)
val iterations = mandelbrot(point)
```

### 3D Rotations with Quaternions
```scala
import io.github.edadma.numbers._

// Rotation quaternion for 90° around Z-axis
val angle = math.Pi / 4  // 45° in radians, but quaternion uses half-angle
val axis = QuaternionDouble(0, 0, 0, 1) // Z-axis
val rotation = QuaternionDouble(math.cos(angle), 0, 0, math.sin(angle))

// Apply rotation to a point
val point = QuaternionDouble(0, 1, 0, 0) // Point on X-axis
val rotated = rotation * point * rotation.conj
```

### Financial Calculations
```scala
import io.github.edadma.numbers._

// Exact monetary calculations with rational numbers
val price = Rational(1299, 100)      // $12.99
val taxRate = Rational(875, 10000)   // 8.75% tax
val quantity = 3

val subtotal = price * quantity      // $38.97 exactly
val tax = subtotal * taxRate         // Exact tax calculation
val total = subtotal + tax           // Exact total

println(s"Subtotal: $$${subtotal.doubleValue}")
println(s"Tax: $$${tax.doubleValue}")  
println(s"Total: $$${total.doubleValue}")
```

## Tests

The library includes comprehensive tests covering:
- Basic arithmetic operations for all number types
- Mathematical function accuracy
- Precision maintenance in calculations
- Edge cases and special values
- Integration with Scala standard library

Run tests with:
```bash
sbt test
```

For cross-platform testing:
```bash
sbt "project numbersJVM" test
sbt "project numbersJS" test  
sbt "project numbersNative" test
```

## Performance Considerations

- **Rational arithmetic**: Slower than floating-point but provides exact results
- **BigDecimal types**: Configurable precision with corresponding performance trade-offs
- **Complex/Quaternion operations**: Optimized implementations with lazy evaluation where beneficial
- **Memory usage**: Consider precision requirements vs. memory constraints for BigDecimal types

## Contributing

This project welcomes contributions from the community. Contributions are accepted using GitHub pull requests.

For a good pull request, please provide:

1. **Clear description**: Include the basic "what" and "why" for the request
2. **Passing tests**: Ensure all existing tests pass and add tests for new features
3. **Test coverage**: Run `sbt coverage test coverageReport` to verify coverage
4. **Documentation**: Update README.md for new features or API changes
5. **Code style**: Run `sbt scalafmtAll` to ensure consistent formatting

### Development Setup

```bash
git clone https://github.com/edadma/numbers.git
cd numbers
sbt compile
sbt test
```

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.
