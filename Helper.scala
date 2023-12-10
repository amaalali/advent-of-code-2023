object test {
  def apply[A](result: A, expected: => A): Unit =
    logger.test(result == expected, result, expected)

  def ignore[A](result: => A, expected: => A): Unit = ()

  def apply[A](name: String, result: A, expected: => A): Unit =
    logger.test(result == expected, result, expected, name)

  def ignore[A](name: String, result: A, expected: => A): Unit = ()

  object where {
    def apply[A](result: => A, tester: A => Boolean, name: String = ""): Unit =
      logger.testWhere(tester(result), result, name)

    def ignore[A](result: => A, tester: A => Boolean, name: String = ""): Unit = ()
  }
}

object run {
  def apply[A](res: => A): Unit =
    logger.log("RUN", res.toString())

  def ignore[A](res: => A): Unit = ()
}

object logger {

  private var isDebugEnabled = false
  private var isInfoEnabled = false
  private var _forceTestErrorMessage = false

  def debugOn() = {
    isDebugEnabled = true
  }

  def debugOff() = {
    isDebugEnabled = false
  }

  def infoOn() = {
    isInfoEnabled = true
  }

  def infoOff() = {
    isInfoEnabled = false
  }

  def forceTestErrorMessage() = {
    _forceTestErrorMessage = true
  }

  def debug(message: => String, marker: String = ""): Unit =
    val m = if (marker.isBlank) marker else s" > ${marker}"
    if (isDebugEnabled) log(s"DEBUG${m}", message)

  def info(message: => String, marker: String = ""): Unit =
    val m = if (marker.isBlank) marker else s" > ${marker}"
    if (isInfoEnabled) log(s"INFO${m}", message)

  def test[A](passed: Boolean, result: A, expected: A, name: String = ""): Unit =
    val m = if (name.isBlank) name else s" > ${name}"
    if (passed) {
      val resultStr: String = result.toString()
      val expectedStr: String = expected.toString()
      if (!_forceTestErrorMessage && ((resultStr.size + expectedStr.size) > 120)) {
        log(s"TEST${m}", s"passed :)")
      } else {
        log(s"TEST${m}", s"passed :) ${expected} was equal to ${result}")
      }
    } else {
      println()
      log(s"TEST${m}", s"FAILED :( expected=[${expected}] but got=[${result}]")
      println()
    }

  def testWhere[A](passed: Boolean, result: A, name: String = ""): Unit =
    val m = if (name.isBlank) s" > ${name}" else name
    if (passed) {
      log(s"TEST${m}", s"passed :) with result ${result}")
    } else {
      log(s"TEST${m}", s"FAILED :( with result ${result}")
    }

  def log(key: String, message: => String): Unit = {
    println(s"[$key] ${message}")
  }
}
