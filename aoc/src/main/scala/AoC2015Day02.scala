object AoC2015Day02 extends App :

  import scala.io.Source
  import scala.util.matching.Regex

  val keyPattern = "[0-9]+".r

  val measurements: List[List[Int]] =
    val parser: String => List[Int] = str =>
      keyPattern
        .findAllMatchIn(str)
        .map(_.matched.toInt)
        .toList

    Source
      .fromFile("aoc/src/main/resources/2015/InputDay02.txt")
      .getLines
      .map(_.trim)
      .map(parser)
      .toList


  // Part 1

  def computeAreas(dimensionList: List[Int]): List[Int] =

    // OMG !!!
    val combinations: List[List[Int]] =
      for {
        (x, i) <- dimensionList.zipWithIndex
        (y, j) <- dimensionList.zipWithIndex
        if i < j
      }
      yield List(x, y)

    combinations.map(_.product)

  val answer1: Int =
    measurements
      .map(computeAreas)
      .map(areasList => areasList.min + 2 * areasList.sum)
      .sum

  println(s"Result part 1: ${answer1}")

  // Part 2

  def computeRibbonLength(boxList: List[Int]): Int =
    val bowLength = boxList.product
    val firstMin  = boxList.min
    val secondMin = boxList.diff(List(firstMin)).min

    bowLength + 2 * (firstMin + secondMin)

  val answer2: Int =
    measurements
      .map(computeRibbonLength)
      .sum

  println(s"Result part 2: ${answer2}")

