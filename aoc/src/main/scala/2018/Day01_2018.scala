object Day01_2018 extends App:
    import scala.io.Source

    val changes: List[Int] = Source.fromFile("aoc/src/main/resources/2018/InputDay01.txt")
        .getLines.toList.map(x => x.toInt) //alternatief: .map(_.toInt)
    

    // Part 1

    println(s"Result part 1: ${changes.sum}")
    //alternatief: changes.foldLeft(0)((acc, elm) => acc + elm)

    // Part 2

    def checkOccurence(curFreq: Int, freqChanges: List[Int], seenFreqs: Set[Int]): Int = 
        if (seenFreqs contains curFreq) curFreq
        else if (freqChanges.isEmpty) checkOccurence(curFreq, changes, seenFreqs)
        else checkOccurence(curFreq+freqChanges.head, freqChanges.tail, seenFreqs.incl(curFreq))

    println(s"Result part 2: ${checkOccurence(0, changes, Set())}")
