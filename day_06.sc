//> using file Helper.scala

import scala.io.Source
import util.chaining.*

/** Your toy boat has a starting speed of zero millimeters per millisecond.
  *
  * For each whole millisecond you spend at the beginning of the race holding down the button, the boat's speed increases by one millimeter
  * per millisecond.
  *
  * a0 = 0 m/s
  * 
  *
  */

case class Record(time: Int, dist: Int)
object Record:
  def apply(tuple: (Int, Int)): Record = Record(tuple._1, tuple._2)

  def parseAll(input: String): Seq[Record] =
    val source = Source.fromString(input).getLines().toList
    val times: Seq[Int] = source.head.replace("Time:", "").split(" ").filterNot(_.isBlank).map(_.toInt)
    val dists: Seq[Int] = source.last.replace("Distance:", "").split(" ").filterNot(_.isBlank).map(_.toInt)

    times.zip(dists).map(Record.apply)

def marginOfError = ???

// s = ut + (a * t^2) / 2, where u = 0, therefore ...
// t^2 = 2s/a
// t = (2 * s / a) ^ 0.5
// ^^ Time taken to travel distance with acceleration


//

// >>>>>>>>>>>>>>>>
// Testing
// >>>>>>>>>>>>>>>>

test(Record.parseAll(example).map(_.time), List(7, 15, 30))
test(Record.parseAll(example).map(_.dist), List(9, 40, 200))
test(Record.parseAll(puzzle).map(_.time), List(44, 89, 96, 91))
test(Record.parseAll(puzzle).map(_.dist), List(277, 1136, 1890, 1768))

// ----------------

// >>>>>>>>>>>>>>>>
// Data
// >>>>>>>>>>>>>>>>
def example: String =
  """Time:      7  15   30
    |Distance:  9  40  200""".stripMargin

def puzzle: String =
  """Time:        44     89     96     91
    |Distance:   277   1136   1890   1768""".stripMargin
