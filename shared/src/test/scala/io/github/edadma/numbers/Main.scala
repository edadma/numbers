package io.github.edadma.numbers

@main def run(): Unit = {
  val q1 = QuaternionDouble(1, 2, 3, 4)
  val q2 = QuaternionDouble(5, 6, 7, 8)

  val product  = q1 * q2
  val quotient = product / q2

  println(q1)
  println(product)
  println(quotient)
}
