object AoC2015Day02 extends App:
    import scala.io.Source
    import scala.util.matching.Regex

    val keyPattern = "[0-9]+".r

    val measurements: List[List[Int]] = Source.fromFile("aoc/src/main/resources/2015/InputDay02.txt")
        .getLines.map(x => keyPattern.findAllMatchIn(x).map(_.toString.toInt).toList).toList


    // Part 1

    

    println(measurements(0))