import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try

private sealed trait TestMethods {
  def apply[A](result: => A, expected: => A): Unit
  def apply[A](name: String, result: => A, expected: => A): Unit
}

object test extends TestMethods {

  override def apply[A](result: => A, expected: => A): Unit =
    Try(result)
      .fold(
        e => logger.test.failed(result, expected, e),
        res => {
          val _e = expected
          res match
            case _r if _r == _e => logger.test.passed(_r, _e)
            case _r             => logger.test.failed(_r, _e)
        }
      )

  override def apply[A](name: String, result: => A, expected: => A): Unit =
    Try(result)
      .fold(
        e => logger.test.failed(result, expected, e, name),
        res => {
          val _e = expected
          res match
            case _r if _r == _e => logger.test.passed(_r, _e, name)
            case _r             => logger.test.failed(_r, _e, name)
        }
      )

  object ignore extends TestMethods {
    override def apply[A](result: => A, expected: => A): Unit = ()
    override def apply[A](name: String, result: => A, expected: => A): Unit = ()
  }

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

  private var isDebugEnabled = AtomicBoolean(false)
  private var isInfoEnabled = AtomicBoolean(false)
  private var isForceTestErrorMessageEnabled = AtomicBoolean(false)

  sealed trait toggle(target: AtomicBoolean) {
    def on() = target.set(true)
    def off() = target.set(false)
    def isEnabled = target.get()
  }

  object debug extends toggle(isDebugEnabled)
  object info extends toggle(isInfoEnabled)
  object test {
    object forceMessage extends toggle(isForceTestErrorMessageEnabled)

    def passed[A](result: A, expected: A, name: String = ""): Unit = {
      val m = if (name.isBlank) name else s" > ${name}"
      val resultStr: String = result.toString()
      val expectedStr: String = expected.toString()
      if (forceMessage.isEnabled) {
        log(s"TEST${m}", s"passed :) ${expected} was equal to ${result}")
      } else {
        log(s"TEST${m}", s"passed :)")
      }
    }

    def failed[A](result: A, expected: A): Unit = {
      println()
      log("TEST", s"FAILED :( expected=[${expected}] got=[${result}]")
      println()
    }

    def failed[A](result: A, expected: A, name: String): Unit = {
      val m = s" > ${name}"
      println()
      log(s"TEST${m}", s"FAILED :( expected=[${expected}] got=[${result}]")
      println()
    }

    def failed[A](result: A, expected: A, e: Throwable, name: String = ""): Unit = {
      val m = if (name.isBlank) name else s" > ${name}"
      println()
      log(s"TEST${m}", s"FAILED :( Got exception. expected=[${expected}] got=[${result}]")
      log(s"    ${m}", s"message=[${e.getMessage()}]")
      log(s"    ${m}", s"  trace=[${e.getStackTrace()}]")
      println()
    }

  }

  def debug(message: => String, marker: String = ""): Unit =
    val m = if (marker.isBlank) marker else s" > ${marker}"
    if (debug.isEnabled) log(s"DEBUG${m}", message)

  def info(message: => String, marker: String = ""): Unit =
    val m = if (marker.isBlank) marker else s" > ${marker}"
    if (info.isEnabled) log(s"INFO${m}", message)

  @deprecated("NOW")
  def testWhere[A](passed: Boolean, result: A, name: String = ""): Unit =
    val m = if (name.isBlank) name else s" > ${name}"
    if (passed) {
      log(s"TEST${m}", s"passed :) with result ${result}")
    } else {
      log(s"TEST${m}", s"FAILED :( with result ${result}")
    }

  def log(key: String, message: String): Unit = {
    println(s"[$key] ${message}")
  }
}
