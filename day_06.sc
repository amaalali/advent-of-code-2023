//> using file Helper.scala
import scala.annotation.tailrec

import scala.io.Source

extension (record: (Int, Int))
  def time: Int = record._1
  def dist: Int = record._2

extension (record: (BigInt, BigInt))
  def time: BigInt = record._1
  def dist: BigInt = record._2

extension (number: BigInt)
  def sqrt: BigInt = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1
    val one = BigInt(1)
    val n1 = next(one, number)

    @tailrec
    def sqrtHelper(n: BigInt, n1: BigInt): BigInt =
      if ((n1 - n).abs <= one) List(n1, n).max
      else sqrtHelper(n1, next(n1, number))

    sqrtHelper(one, n1)
  }

def parseAll(input: String): Seq[(Int, Int)] =
  val source = Source.fromString(input).getLines()
  val times = source.next.replace("Time:", "").split(" ").filterNot(_.isBlank).map(_.toInt)
  val dists = source.next.replace("Distance:", "").split(" ").filterNot(_.isBlank).map(_.toInt)
  times.zip(dists)

def parseOne(input: String): (BigInt, BigInt) =
  val source = Source.fromString(input).getLines()
  val time = BigInt(source.next.replace("Time:", "").replace(" ", ""))
  val dist = BigInt(source.next.replace("Distance:", "").replace(" ", ""))
  (time, dist)

def roots(record: (Int, Int)): Seq[Int] =
  val discriminant = (record.time * record.time) - (4 * record.dist)
  val discriminantRoot = Math.pow(discriminant, 0.5)
  val denom = 2

  val pNumerator = record.time + discriminantRoot
  val nNumerator = record.time - discriminantRoot

  val pRoundDown = (pNumerator % denom == 0)
  val nRoundUp = true

  val pRes = (pNumerator / denom).toInt
  val nRes = (nNumerator / denom).toInt

  val one = if (nRoundUp) nRes + 1 else nRes
  val two = if (pRoundDown) pRes - 1 else pRes

  List(one, two)

def rootsBigInt(record: (BigInt, BigInt)): Seq[BigInt] =
  val discriminant = (record.time * record.time) - (4 * record.dist)
  val discriminantRoot = discriminant.sqrt
  val denom = 2

  val pNumerator = record.time + discriminantRoot
  val nNumerator = record.time - discriminantRoot

  val pRoundDown = (pNumerator % denom == 0)
  val nRoundUp = true

  val pRes = (pNumerator / denom).toInt
  val nRes = (nNumerator / denom).toInt

  val one = if (nRoundUp) nRes + 1 else nRes
  val two = if (pRoundDown) pRes - 1 else pRes

  List(one, two)

def solSize[A](solRng: Seq[A])(using num: Numeric[A]): A =
  import num._

  solRng match
    case Nil         => num.fromInt(0)
    case _ :: Nil    => num.fromInt(1)
    case h :: t :: _ => t - h + num.fromInt(1)

def run1(input: String): Int =
    parseAll(input)
    .map(roots andThen solSize)
    .product

def run2(input: String): BigInt =
  (parseOne andThen rootsBigInt andThen solSize)(input)

/*
 **************************
 * TESTING
 **************************
 */

// test.focus.on()
// logger.debug.on()
run.off()

test("example parse times", parseAll(example).map(_.time), List(7, 15, 30))
test("example parse dists", parseAll(example).map(_.dist), List(9, 40, 200))

test("puzzle parse times", parseAll(puzzle).map(_.time), List(44, 89, 96, 91))
test("puzzle parse dists", parseAll(puzzle).map(_.dist), List(277, 1136, 1890, 1768))

test("roots example 1", roots((7, 9)), List(2, 5))
test("roots example 2", roots((15, 40)), List(4, 11))
test("roots example 3", roots((30, 200)), List(11, 19))

test("solCount example 1", solSize(List(2, 5)), 4)
test("solCount example 2", solSize(List(4, 11)), 8)
test("solCount example 3", solSize(List(11, 19)), 9)

test(run1(example), 288)

run(run1(puzzle))
test(run1(puzzle), 2344708)

test("puzzle parse dists for Kerning", parseOne(example), (71530, 940200))

test(run2(example), 71503)

run(run2(puzzle))
test(run2(puzzle), 30125202)

/*
 **************************
 * DATA
 **************************
 */

def example: String =
  """Time:      7  15   30
    |Distance:  9  40  200""".stripMargin

def puzzle: String =
  """Time:        44     89     96     91
    |Distance:   277   1136   1890   1768""".stripMargin
