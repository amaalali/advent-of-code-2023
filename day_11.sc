//> using file Helper.scala
import logger.log
import util.chaining.scalaUtilChainingOps

type Galaxy = (Int, Int)
type Galaxies = Seq[Galaxy]

type BigGalaxy = (BigInt, BigInt)
type BigGalaxies = Seq[BigGalaxy]

def parseGalaxies(input: String): Galaxies = {
  io.Source
    .fromString(input)
    .getLines()
    .zipWithIndex
    .flatMap((ln, yIdx) =>
      ln.zipWithIndex
        .filter(_._1 == '#')
        .map((_, xIdx) => (xIdx + 1, yIdx + 1))
    )
    .toList
}

def measureImageSize(input: String): (Int, Int) = {
  val asdf = io.Source
    .fromString(input)
    .getLines()

  val xLen = asdf.next().length()
  val yLen = asdf.size + 1

  (xLen, yLen)
}

def findGaps(len: Int, elems: Seq[Int]): Seq[Int] = {
  elems
    .foldLeft((Vector.empty[Int], 1)) {

      case ((acc, last), next) if (Math.abs(last - next) <= 1) =>
        (acc, next)

      case ((acc, last), next) =>
        val nxt = (acc :+ last :+ next, next)
        nxt

    }
    ._1
    .pipe(rngs => {
      (elems.head != 1, elems.last != len) match {
        case (true, true)   => Vector(0, rngs.head) ++ rngs :+ rngs.last :+ (len + 1)
        case (true, false)  => Vector(0, rngs.head) ++ rngs
        case (false, true)  => rngs :+ rngs.last :+ (len + 1)
        case (false, false) => rngs
      }
    })
    .sliding(2, 2)
    .flatMap(rngLs =>
      val start = rngLs.head
      val end = rngLs.last
      ((start + 1) until end).toList
    )
    .toList
}

type ExpansionLines = (Seq[Int], Seq[Int])
def findExpansionLines(xLen: Int, yLen: Int)(galaxies: Galaxies): ExpansionLines = {
  val xIdxs = galaxies.map(_._1).toSet.toList.sorted
  val yIdxs = galaxies.map(_._2).toSet.toList.sorted

  (
    findGaps(xLen, xIdxs),
    findGaps(yLen, yIdxs)
  )
}

def adjustForGravitationalDrift1(originalImageGalaxies: Galaxies, expansionLines: ExpansionLines): Galaxies = {
  val xExpansions = expansionLines._1
  val yExpansions = expansionLines._2

  originalImageGalaxies.map((x_i, y_i) => {
    val xIncr = xExpansions.size - xExpansions.filter(_ > x_i).size
    val x_n = x_i + xIncr

    val yIncr = yExpansions.size - yExpansions.filter(_ > y_i).size
    val y_n = y_i + yIncr

    (x_n, y_n)
  })
}

def adjustForGravitationalDriftX(multiplier: Int, originalImageGalaxies: Galaxies, expansionLines: ExpansionLines): BigGalaxies = {
  val xExpansions = expansionLines._1
  val yExpansions = expansionLines._2

  originalImageGalaxies.map((x_i, y_i) => {
    val xExpSize = xExpansions.size
    val yExpSize = yExpansions.size

    val xExpCount = xExpansions.filter(_ > x_i).size
    val yExpCount = yExpansions.filter(_ > y_i).size

    val xIncr = BigInt(xExpSize - xExpCount) * BigInt(multiplier - 1)
    val x_n = x_i + xIncr

    val yIncr = BigInt(yExpSize - yExpCount) * BigInt(multiplier - 1)
    val y_n = y_i + yIncr

    logger.debug(s"(x,y)=[${(x_i)},${y_i}] x_i=[${x_i}] xExpSize=[${xExpSize}] xExpCount=[${xExpCount}] inc=[${xIncr}]")

    (x_n, y_n)
  })
}

def dist[A](g1: (A, A), g2: (A, A))(implicit numA: Numeric[A]): A =
  numA.plus(
    numA.abs(numA.minus(g1._1, g2._1)),
    numA.abs(numA.minus(g1._2, g2._2))
  )

def distToNearestNeighbour(galaxies: Galaxies): Seq[Int] =
  galaxies
    .combinations(2)
    .foldLeft(Map.empty[Set[Galaxy], Int]) { (distances, comb) =>
      val next: (Set[Galaxy], Int) = (comb.toSet, dist(comb.head, comb.last))
      distances + next
    }
    .values
    .toList

def distToNearestNeighbourBig(galaxies: BigGalaxies): Seq[BigInt] =
  galaxies
    .combinations(2)
    .foldLeft(Map.empty[Set[BigGalaxy], BigInt]) { (distances, comb) =>
      val next: (Set[BigGalaxy], BigInt) = (comb.toSet, dist(comb.head, comb.last))
      distances + next
    }
    .values
    .toList

def run1(input: String): Int =
  val (xLen, yLen) = measureImageSize(input)
  val initialGalaxies = parseGalaxies(input)
  val lines = findExpansionLines(xLen, yLen)(initialGalaxies)
  val adjustedGalaxies = adjustForGravitationalDrift1(initialGalaxies, lines)
  distToNearestNeighbour(adjustedGalaxies).sum

def run2(emptySpaceMultiplier: Int, input: String): BigInt = {

  val initialGalaxies = parseGalaxies(input)

  val (xLen, yLen) = measureImageSize(input)
  val lines = findExpansionLines(xLen, yLen)(initialGalaxies)

  val adjustedGalaxies = adjustForGravitationalDriftX(emptySpaceMultiplier, initialGalaxies, lines)

  logger.debug(lines._1.toString, "xAdj")
  logger.debug(lines._2.toString, "yAdj")
  logger.debug(initialGalaxies.toString, "init")
  logger.debug(adjustedGalaxies.toString, "adj ")

  distToNearestNeighbourBig(adjustedGalaxies).sum
}
/*
 **************************
 * TESTING
 **************************
 */

test(
  parseGalaxies(example),
  List((4, 1), (8, 2), (1, 3), (7, 5), (2, 6), (10, 7), (8, 9), (1, 10), (5, 10))
)

test(
  parseGalaxies(exampleExpanded),
  List(
    (5, 1),
    (10, 2),
    (1, 3),
    (9, 6),
    (2, 7),
    (13, 8),
    (10, 11),
    (1, 12),
    (6, 12)
  )
)


val (xLen, yLen) = measureImageSize(example)

test(
  (parseGalaxies andThen findExpansionLines(xLen, yLen))(example),
  (List(3, 6, 9), List(4, 8))
)

test(
  findGaps(5, List(1, 2, 5)),
  List(3, 4)
)

test(
  findGaps(6, List(2, 5)),
  List(1, 3, 4, 6)
)

test(
  {
    val (xLen, yLen) = measureImageSize(example)
    val galaxies = parseGalaxies(example)
    val lines = findExpansionLines(xLen, yLen)(galaxies)

    adjustForGravitationalDrift1(galaxies, lines).groupBy(_._2)
  },
  parseGalaxies(exampleExpanded).groupBy(_._2)
)

test(
  dist((2, 7), (6, 12)),
  9
)

test(
  distToNearestNeighbour(Seq((2, 7), (6, 12))),
  List(9)
)

test(
  run1(example),
  374
)

run.ignore(run1(puzzle))
test(
  run1(puzzle),
  9599070
)

test(
  "run2 x 10 ",
  // run2(10, example) - 82,
  run2(10, example),
  1030
)

test(
  "run2 x 100",
  run2(100, example),
  8410
)

run.ignore(run2(1000000, puzzle))
test.where(run2(1000000, puzzle), !List(BigInt("842646756350"), BigInt("842646756432")).contains(_), "This is deffo no the answer")
test(run2(1000000, puzzle), BigInt("842645913794"))


/*
 **************************
 * DATA
 **************************
 */
def example: String =
  """...#......
    |.......#..
    |#.........
    |..........
    |......#...
    |.#........
    |.........#
    |..........
    |.......#..
    |#...#.....""".stripMargin

def exampleExpanded: String =
  """....#........
    |.........#...
    |#............
    |.............
    |.............
    |........#....
    |.#...........
    |............#
    |.............
    |.............
    |.........#...
    |#....#.......""".stripMargin

def puzzle: String =
  """..............................#.............#.....#.............#...........#......................................#...............#........
    |....................#.........................................................................#.........#...................................
    |...........#............................#...............................................#...................................................
    |......................................................................#...........................#.........................................
    |.#.......................................................#.....................................................#.............#..............
    |......................#.............#..............................................#.......#..........#.....................................
    |............................................................................................................................................
    |................#..........#................#................#................#........#..................................#.................
    |...................................................#........................................................#......................#........
    |............................................................................................................................................
    |.....#............................................................#.........................................................................
    |................................#.......................................#........#.............#........#.....................#.............
    |...........................................#.......................................................................#........................
    |..........#...........................................................................................................................#.....
    |.#.................................................#.....................................................................#..................
    |............................#......#.........................................#..............#................#..............................
    |..............#..........................................#........#...............................#.........................................
    |...................#.......................................................................................................................#
    |.........#..................................................................................................................................
    |..........................................................................................................#................#................
    |..#..........................................#.................................#.......#......#.......................#.....................
    |..................................................#.........................................................................................
    |.......#................#................................................#..................................................................
    |.............................#...........#...................#......................#............#...............................#.........#
    |.............#......................................................................................................#.......................
    |..............................................#................................................................#............................
    |.....................................#............................#..........#...........................#.................#................
    |.....................#......................................................................................................................
    |#.........................................#.......................................................#...............................#.........
    |......................................................................#.....................................................................
    |...................................#..................#........#....................#............................#..........................
    |...................#............................#...........................................................................................
    |...................................................................#....................#.....................................#........#....
    |.........#...............................#.....................................#.......................................#....................
    |.#..........................................................................................................................................
    |............................................................................................#......................#.......................#
    |................#..............#.............................#.......................#............#................................#........
    |.....................#............................#...............#.........................................................................
    |........................................................#.......................#.........................................#.................
    |...........................#..........................................#...............................#.....................................
    |.....................................#.....................................................................#................................
    |...........#.......#.............................................................................................................#.......#..
    |.....#........................#..................#..................................................................#.......................
    |...........................................#................................#...............................................................
    |..............................................................#.............................................................................
    |........................................................#............#...............#......................#...............................
    |.#..........................................................................................................................................
    |....................#...........#.......................................................................................#...................
    |......................................#..............#...........#.................................................#...................#....
    |.......#.....#..........#....................................................................#.................................#............
    |.............................#..............#...............................................................................................
    |............................................................................................................................................
    |#............................................................#.....................................#.....................................#..
    |....................#..................................#...........................#.....................#..................................
    |............................................................................................................................................
    |............#..........................................................#.............................................................#......
    |............................#...................................#.............#.................#.............#......#......#...............
    |..................#...........................#.............................................................................................
    |.......................#...........#.................................................#............................................#.........
    |............................................................................................................................................
    |..............................#.........#.................................#............................................................#....
    |............................................................................................................................................
    |....#.................................................................#...................................#..............#..................
    |...........#..................................................#...................................#.............#...........................
    |................#...........................#...............................................................................................
    |.#.....................#...............................#...........#...........#.....#.............................................#........
    |............................................................................................................................................
    |........#........................#.....................................#...............................................#....................
    |............................................................................................................................................
    |................................................................................................................#...........................
    |............................................................................................................................................
    |......................................#..............#.........#...........#...........#...............#....................................
    |...........#.......................................................................................................#.................#......
    |......#.....................................#.......................#......................#..............................................#.
    |..............................................................................#..............................................#..............
    |...............................#...........................#................................................#...............................
    |................................................#...................................................#............#...............#..........
    |............#............................#................................#.............#................................#..................
    |.................#.......#..................................................................................................................
    |....................................#.................................#..............................................#...............#......
    |.........#..................................................................................................................................
    |.............................#.................................................................................#............................
    |......................#...................#..................................................#.........#....................................
    |............................................................................................................................................
    |#..................................................#........................................................................................
    |........#.......................................................................#.................................#.......#.................
    |.............#.........................................#............#..............................................................#........
    |.....................................#........................#......................#............#.........................................
    |.........................#.......................#..........................#...........................#......................#............
    |.................................#.........#................................................................................................
    |...................#.........................................................................#..........................#...................
    |............................................................................................................................................
    |..............................#....................#............#.........................................................................#.
    |.........................................................................................................#.......#.........#................
    |..........#.......................................................................................................................#.........
    |....#.............#.......#.........................................#............#..........................................................
    |...................................#........................................................................................................
    |...........................................................#............................................................#...................
    |#..............................#.............#.....#.........................#.......#...............#........................#.....#.......
    |................................................................................................#........................................#..
    |........#...............#................................................................#...........................#......................
    |...................#.....................................#.......................#.........................................#................
    |............................................................................................................................................
    |..............#..................................................#...........................#...................#..........................
    |...............................#..............#...........................#.................................................................
    |#..........................................................#........................#..........................................#.......#....
    |.....................................................#....................................................#.........#.......................
    |..........................#................................................................#......#.......................#.................
    |....#.....#.....................................................................................................#...........................
    |.....................#.....................#..........................................#.....................................................
    |.........................................................................#..............................................................#...
    |..............#......................................................................................#........................#.............
    |...........................#..........#............#...............................................................#........................
    |........#....................................#..............................................................................................
    |.................................................................#..........................#....................................#..........
    |.................................................................................#.....................#.....#...........................#..
    |............................................................................................................................................
    |..........................#.................................................................................................................
    |..............................................................................#.............................................................
    |.........................................................................#...................#......................................#.......
    |...#.........#......#................................#..................................................................#...................
    |...............................#.............#...............#.......................#...........................#..........................
    |.......................................................................................................#.........................#..........
    |............................................................................#...............................................................
    |.#....................#.............................................#....................................................................#..
    |....................................#..................#....................................................................................
    |.....#........................#...............#..........................#...................................#..............................
    |...........#........................................................................................#.......................................
    |................#.........#................................#.....#..............................................................#...........
    |......................................#..............................................#.................................................#....
    |.................................................................................................#......#........#.....#....................
    |#......................#.............................................#......................................................................
    |..............#..........................#..............#...................................................................................
    |...........................................................................#................................................................
    |..................#..............#.................#..............#...........................................#.....................#.......
    |............................................................................................#...............................................
    |..........................#.............................................#........#........................#.................................
    |..........#...........................................#.....................................................................................
    |......................................#........................#...............................#...........................................#
    |.....#........#..............#.......................................#.............................................#..........#.............""".stripMargin
