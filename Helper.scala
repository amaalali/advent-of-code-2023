object test {
  def apply[A](result: A, expected: => A): Unit = println(s"""[TEST] ${
      if (result == expected) "passed :) " else "FAILED"
    } expected ${expected} got ${result}""")

  def apply[A](name: String)(result: A, expected: => A): Unit = println(s"""[TEST][$name] ${
      if (result == expected) "passed :) " else "FAILED"
    } expected ${expected} got ${result}""")

  def where[A](result: => A, tester: A => Boolean): Unit = println(s"""[TEST] ${
      if (tester(result)) "passed :) " else "FAILED"
    } with result ${result}""")

  def ignore[A](result: => A, expected: => A): Unit = ()
}

object run {
  def apply[A](fn: String => A)(input: String): Unit =
    println(s"""[RUN] ${fn(input)}""")

}
