object AoC2015Day05 extends App:

    import scala.io.Source

    val strings: List[String] = 
        Source.fromResource("2015/InputDay05.txt")
        .getLines
        .toList

    case class Word(str: String):
        val vowels: Set[Char] = Set('a', 'e', 'i', 'o', 'u')

        val prohibitedStrings: Set[String] = Set("ab", "cd", "pq", "xy")

        def threeVowels(): Boolean =
            str.filter(char => vowels.contains(char)).length >= 3
        
        def twoLettersInRow(): Boolean =
            str.sliding(2).map(s => s(0) == s(1)).contains(true)

        def noProhibitedSubstrings(): Boolean =
            ! str.sliding(2).map(s => prohibitedStrings.contains(s)).contains(true)

        def isNice1(): Boolean =
            threeVowels() & twoLettersInRow() & noProhibitedSubstrings()

        def hasRecurringPair(): Boolean =
            val pairsWithIndices: List[(String, (Int, Int))] =
                str.sliding(2)
                .zipWithIndex
                .map((s, idx) => (s, (idx, idx+1)))
                .toList

            println(pairsWithIndices)
            
            val pairsGrouped = pairsWithIndices
            .groupBy(_(0))
            .map((pair, pairsWithIdxList) => (pair, pairsWithIdxList(0).toList))//.fold(Set())((set, idx) => set.include(idx))))

            println(pairsGrouped.toList)

            true

    //val answer1: Int = strings.map(Word(_).isNice1()).count(_ == true)

    //println(s"Result part 1: ${answer1}")

    println("xyxy".sliding(2).zipWithIndex.map((s, idx) => (s, (idx, idx+1))).toList)//(0)(0))
