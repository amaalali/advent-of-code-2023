//> using toolkit latest
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try
import logger.logEmptyLine
import test.focus

private sealed trait TestMethods {
  def apply[A](result: => A, expected: => A): Unit =
    Try(result)
      .fold(
        e => logger.test.failed.resultExpectedNamedWithThrow("", result, expected, e),
        res => {
          val _e = expected
          res match
            case _r if _r == _e => logger.test.passed.resultExpectedAnon(_r, _e)
            case _r             => logger.test.failed.resultExpectedAnon(_r, _e)
        }
      )

  def apply[A](name: String, result: => A, expected: => A): Unit =
    Try(result)
      .fold(
        e => logger.test.failed.resultExpectedNamedWithThrow("", result, expected, e),
        res => {
          val _e = expected
          res match
            case _r if _r == _e => logger.test.passed.resultExpectedNamed(name, _r, _e)
            case _r             => logger.test.failed.resultExpectedNamed(name, _r, _e)
        }
      )
}

private sealed trait Toggle {
  protected val isEnabledFlag = AtomicBoolean(false)
  def on() = isEnabledFlag.set(true)
  def off() = isEnabledFlag.set(false)
  def isEnabled = isEnabledFlag.get()
  def isDisabled = !isEnabledFlag.get()
}

object ftest extends TestMethods {
  override def apply[A](result: => A, expected: => A): Unit =
    test.focus.apply(result, expected)

  override def apply[A](name: String, result: => A, expected: => A): Unit =
    test.focus.apply(name, result, expected)
}

object test extends TestMethods {

  object focus extends Toggle with TestMethods {
    private val focusWarningMessage = "Focus mode is disabled, however the tests below was flagged for focus."
    override def apply[A](result: => A, expected: => A): Unit = {
      if (!focus.isEnabled) {
        logEmptyLine()
        logger.log("WARN", focusWarningMessage)
        super.apply(result, expected)
        logEmptyLine()
      } else {
        super.apply(result, expected)
      }
    }

    override def apply[A](name: String, result: => A, expected: => A): Unit = {
      if (!focus.isEnabled) {
        logEmptyLine()
        logger.log("WARN", focusWarningMessage)
        super.apply(name, result, expected)
        logEmptyLine()
      } else {
        super.apply(name, result, expected)
      }
    }

  }

  override def apply[A](result: => A, expected: => A): Unit =
    if (focus.isDisabled) super.apply(result, expected)

  override def apply[A](name: String, result: => A, expected: => A): Unit =
    if (focus.isDisabled) super.apply(name, result, expected)

  object ignore extends TestMethods {
    override def apply[A](result: => A, expected: => A): Unit = ()
    override def apply[A](name: String, result: => A, expected: => A): Unit = ()
  }

  object where {
    def apply[A](name: String, result: => A, tester: A => Boolean): Unit =
      if (focus.isDisabled)
        Try(tester(result))
          .fold(
            e => logger.test.failed.namedWithThrow(name, e),
            {
              case true  => logger.test.passed.messageOnly()
              case false => logger.test.failed.messageOnly()
            }
          )

    def ignore[A](name: String, result: => A, tester: A => Boolean): Unit = ()
  }
}

object run extends Toggle {
  def apply[A](res: => A): Unit =
    if (isEnabled)
      logger.log("RUN", res.toString())

  def ignore[A](res: => A): Unit = ()
}

private sealed trait LoggingI {
  def messageOnly(): Unit

  def resultOnlyAnon[A](result: A): Unit
  def resultOnlyNamed[A](name: String, result: A): Unit

  def resultExpectedAnon[A](result: A, expected: A): Unit
  def resultExpectedNamed[A](name: String, result: A, expected: A): Unit
}

object logger {

  object debug extends Toggle
  object info extends Toggle

  object test {

    object forceMessage extends Toggle

    object passed extends LoggingI {

      def messageOnly(): Unit =
        log(s"TEST", s"passed :)")

      def resultOnlyAnon[A](result: A): Unit = {
        val resultStr: String = result.toString()
        if (logger.test.forceMessage.isEnabled) {
          log(s"TEST", s"passed :) got=[${result}]")
        } else {
          messageOnly()
        }
      }

      def resultOnlyNamed[A](name: String, result: A): Unit = {
        val m = if (name.isBlank) name else s" > ${name}"
        val resultStr: String = result.toString()
        if (logger.test.forceMessage.isEnabled) {
          log(s"TEST${m}", s"passed :) got=[${result}]")
        } else {
          messageOnly()
        }
      }

      def resultExpectedAnon[A](result: A, expected: A): Unit = {
        val resultStr: String = result.toString()
        val expectedStr: String = expected.toString()
        if (logger.test.forceMessage.isEnabled) {
          log(s"TEST", s"passed :) ${expected} was equal to ${result}")
        } else {
          messageOnly()
        }
      }

      def resultExpectedNamed[A](name: String, result: A, expected: A): Unit =
        val m = if (name.isBlank) name else s" > ${name}"
        val resultStr: String = result.toString()
        val expectedStr: String = expected.toString()
        if (logger.test.forceMessage.isEnabled) {
          log(s"TEST${m}", s"passed :) ${expected} was equal to ${result}")
        } else {
          messageOnly()
        }
    }

    object failed extends LoggingI {
      def messageOnly(): Unit =
        log("TEST", s"FAILED :(")

      def resultOnlyAnon[A](result: A): Unit = {
        logEmptyLine()
        log("TEST", s"FAILED :( got=[${result}]")
        logEmptyLine()
      }

      def namedWithThrow(name: String, e: Throwable): Unit = {
        logEmptyLine()
        messageOnly()
        log(s"        ", s"message=[${e.getMessage()}]")
        log(s"        ", s"  trace=${e.getStackTrace()}")
        logEmptyLine()
      }

      def resultOnlyAnonWithThrow[A](result: A, e: Throwable): Unit = {
        logEmptyLine()
        log("TEST", s"FAILED :( got=[${result}]")
        log(s"        ", s"message=[${e.getMessage()}]")
        log(s"        ", s"  trace=${e.getStackTrace()}")
        logEmptyLine()
      }

      def resultOnlyNamed[A](name: String, result: A): Unit = {
        val m = if (name.isBlank) name else s" > ${name}"
        logEmptyLine()
        log(s"TEST${m}", s"FAILED :( got=[${result}]")
        logEmptyLine()
      }

      def resultExpectedAnon[A](result: A, expected: A): Unit = {
        logEmptyLine()
        log("TEST", s"FAILED :( expected=[${expected}] got=[${result}]")
        logEmptyLine()
      }

      def resultExpectedNamed[A](name: String, result: A, expected: A): Unit = {
        val m = s" > ${name}"
        logEmptyLine()
        log(s"TEST${m}", s"FAILED :( expected=[${expected}] got=[${result}]")
        logEmptyLine()
      }

      def resultExpectedNamedWithThrow[A](name: String, result: A, expected: A, e: Throwable): Unit = {
        val m = if (name.isBlank) name else s" > ${name}"
        logEmptyLine()
        log(s"TEST${m}", s"FAILED :( Got exception. expected=[${expected}] got=[${result}]")
        log(s"    ${m}", s"message=[${e.getMessage()}]")
        log(s"    ${m}", s"  trace=[${e.getStackTrace()}]")
        logEmptyLine()
      }
    }

  }

  def debug(message: => String, marker: String = ""): Unit =
    val m = if (marker.isBlank) marker else s" > ${marker}"
    if (debug.isEnabled) log(s"DEBUG${m}", message)

  def info(message: => String, marker: String = ""): Unit =
    val m = if (marker.isBlank) marker else s" > ${marker}"
    if (info.isEnabled) log(s"INFO${m}", message)

  def error(message: => String, marker: String = ""): Unit =
    val m = if (marker.isBlank) marker else s" > ${marker}"
    log(s"ERR ${m}", message)

  def log(key: String, message: String): Unit = {
    println(s"[$key] ${message}")
  }

  object write {

    def apply(fileName: String, data: String): Unit = {
      val str = data + System.lineSeparator()
      os.write.over(os.pwd / fileName, str)
    }

    object append {

      def apply(fileName: String, data: String): Unit = {
        val str = data + System.lineSeparator()
        os.write.append(os.pwd / fileName, str)
      }

    }
  }

  def logEmptyLine(): Unit = println()

}
