package io.github.edadma.numbers

trait QuaternionDoubleIsFractional extends Fractional[QuaternionDouble] {

  def plus(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x + y

  def minus(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x - y

  def times(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x * y

  def div(x: QuaternionDouble, y: QuaternionDouble): QuaternionDouble = x / y

  def negate(x: QuaternionDouble): QuaternionDouble = -x

  def parseString(str: String): Option[QuaternionDouble] = {
    val trimmed = str.trim
    if (trimmed.isEmpty) return None

    // Check if this contains quaternion-specific units (j or k)
    // If not, it's just a real or complex number, not a quaternion
    if (!trimmed.contains('j') && !trimmed.contains('k')) {
      return None
    }

    // Try full form first (a±bi±cj±dk)
    import QuaternionDoubleIsFractional.QUATERNION_FULL
    trimmed match {
      case QUATERNION_FULL(a, signB, b, signC, c, signD, d) =>
        // Handle coefficients that might be null or empty (for optional groups)
        val bCoeff = if (b == null || b.isEmpty) 1.0 else b.toDouble
        val cCoeff = if (c == null || c.isEmpty) 1.0 else c.toDouble
        val dCoeff = if (d == null || d.isEmpty) 1.0 else d.toDouble

        // Apply signs
        val bVal = if (signB == "-") -bCoeff else bCoeff
        val cVal = if (signC == "-") -cCoeff else cCoeff
        val dVal = if (signD == "-") -dCoeff else dCoeff

        return Some(QuaternionDouble(a.toDouble, bVal, cVal, dVal))
      case _ => // Fall through to partial parsing
    }

    // Parse partial forms
    parsePartialQuaternion(trimmed)
  }

  private def parsePartialQuaternion(str: String): Option[QuaternionDouble] = {
    import QuaternionDoubleIsFractional.TERM_PATTERN

    // Initialize components
    var a, b, c, d = 0.0

    // Handle pure unit cases first
    str match {
      case "j" => return Some(QuaternionDouble(0, 0, 1, 0))
      case "-j" => return Some(QuaternionDouble(0, 0, -1, 0))
      case "k" => return Some(QuaternionDouble(0, 0, 0, 1))
      case "-k" => return Some(QuaternionDouble(0, 0, 0, -1))
      case "i" => return Some(QuaternionDouble(0, 1, 0, 0))
      case "-i" => return Some(QuaternionDouble(0, -1, 0, 0))
      case _ => // Continue with term parsing
    }

    // Split the string into terms, preserving signs
    val terms = splitIntoTerms(str)

    for (term <- terms) {
      val cleanTerm = term.trim
      if (cleanTerm.nonEmpty) {
        cleanTerm match {
          case TERM_PATTERN(sign, coeff, unit) =>
            val coefficient = if (coeff == null || coeff.isEmpty) 1.0 else coeff.toDouble
            val value = if (sign == "-") -coefficient else coefficient

            unit match {
              case null | "" => a = value // Real part
              case "i" => b = value
              case "j" => c = value
              case "k" => d = value
              case _ => return None // Invalid unit
            }
          case _ =>
            // Try parsing as just a real number (no unit)
            try {
              if (!cleanTerm.contains('i') && !cleanTerm.contains('j') && !cleanTerm.contains('k')) {
                a = cleanTerm.toDouble
              } else {
                return None // Invalid format
              }
            } catch {
              case _: NumberFormatException => return None
            }
        }
      }
    }

    Some(QuaternionDouble(a, b, c, d))
  }

  private def splitIntoTerms(str: String): List[String] = {
    // Handle leading sign by treating it as part of first term
    val normalizedStr = if (str.startsWith("+")) str.substring(1) else str

    val terms = scala.collection.mutable.ListBuffer[String]()
    var current = ""
    var i = 0

    while (i < normalizedStr.length) {
      val char = normalizedStr(i)
      if ((char == '+' || char == '-') && i > 0 && current.nonEmpty) {
        terms += current
        current = char.toString
      } else {
        current += char
      }
      i += 1
    }

    if (current.nonEmpty) {
      terms += current
    }

    terms.toList
  }

  def fromInt(x: Int): QuaternionDouble = x

  def toInt(x: QuaternionDouble): Int = x.intValue

  def toLong(x: QuaternionDouble): Long = x.longValue

  def toFloat(x: QuaternionDouble): Float = x.floatValue

  def toDouble(x: QuaternionDouble): Double = x.doubleValue

  def compare(x: QuaternionDouble, y: QuaternionDouble): Int = sys.error("can't compare quaternion numbers")

}

object QuaternionDoubleIsFractional {

  // Full form quaternion: a±bi±cj±dk (all components present)
  // This regex captures the signs of i, j, k coefficients to fix the original bug
  private val QUATERNION_FULL =
    """(-?(?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)([+-])((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?i([+-])((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?j([+-])((?:0|[1-9]\d*)(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)?k""".r

  // Pattern for parsing individual terms: [sign][coefficient][unit]
  // Matches things like: "2i", "-3j", "k", "1.5", "-0.7k", "4.2e-3i"
  private val TERM_PATTERN = """([+-]?)(\d*(?:\.\d*)?(?:[eE](?:\+|-|)\d+)?)(i|j|k)?""".r

  implicit object quaternionDoubleIsFractional extends QuaternionDoubleIsFractional

}
