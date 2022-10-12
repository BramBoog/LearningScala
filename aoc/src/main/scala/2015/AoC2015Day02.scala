object AoC2015Day02 extends App:
    import scala.io.Source
    import scala.util.matching.Regex

    val keyPattern = "[0-9]+".r

    val measurements: List[List[Int]] = Source.fromFile("aoc/src/main/resources/2015/InputDay02.txt")
        .getLines.map(x => keyPattern.findAllMatchIn(x).map(_.toString.toInt).toList).toList


    // Part 1
    
    def computeAreas(dimensionList: List[Int]): List[Int] =
        val combinations: List[List[Int]] = 
            for {
                (x, i) <- dimensionList.zipWithIndex
                (y, j) <- dimensionList.zipWithIndex
                if i < j
            }
            yield List(x, y)
        
        combinations.map(lst => lst.fold(1)((x,y) => x*y)).toList

    val totalAreaPerBox: List[Int] = measurements.map(computeAreas).map(areasList => areasList.min + 2*areasList.fold(0)((x,y) => x+y))

    println(s"Result part 1: ${totalAreaPerBox.sum}")

    // Part 2

    def computeRibbonLength(boxList: List[Int]): Int =
        val bowLength = boxList.fold(1)((x,y) => x*y)
        
        val firstMin: Int = boxList.min
        val secondMin: Int = boxList.diff(List(firstMin)).min

        bowLength + 2*(firstMin + secondMin)

    val ribbonLengthPerBox: List[Int] = measurements.map(computeRibbonLength)

    println(s"Result part 2: ${ribbonLengthPerBox.sum}")
