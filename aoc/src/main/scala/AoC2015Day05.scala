object AoC2015Day05 extends App:

    import scala.io.Source

    val strings: List[String] = 
        Source.fromResource("2015/InputDay05.txt")
        .getLines
        .toList

    case class Word(str: String):

        val pairs: List[String] = str.sliding(2).toList

        // Part 1

        val vowels: Set[Char] = Set('a', 'e', 'i', 'o', 'u')

        val prohibitedStrings: Set[String] = Set("ab", "cd", "pq", "xy")

        def threeVowels(): Boolean =
            str.count(vowels.contains) >= 3
        
        def twoLettersInRow(): Boolean =
            pairs.count(s => s(0) == s(1)) >= 1

        def noProhibitedSubstrings(): Boolean =
            !(pairs.count(prohibitedStrings.contains) >= 1)

        def isNice1(): Boolean =
            threeVowels() & twoLettersInRow() & noProhibitedSubstrings()

        // Part 2

        def hasRecurringPair(): Boolean =
            val pairsWithIndices: List[(String, (Int, Int))] =
                pairs
                .zipWithIndex
                .map((s, idx) => (s, (idx, idx+1)))
                .toList
            
            pairsWithIndices
            .groupBy(_._1)
            .map((pair, listOfPairsWithIndices) => listOfPairsWithIndices.map((pair, indices) => indices.toList).flatten.toSet)
            .count(_.size >= 4) >= 1

        def repeatingLetterSeparatedByOne(): Boolean =
            str.sliding(3).count(s => s(0) == s(2)) >= 1

        def isNice2(): Boolean =
            hasRecurringPair() & repeatingLetterSeparatedByOne()

    val words: List[Word] = strings.map(Word(_))

    val answer1: Int = words.map(_.isNice1()).count(_ == true)

    println(s"Result part 1: ${answer1}")

    val answer2: Int = words.map(_.isNice2()).count(_ == true)

    println(s"Result part 2: ${answer2}")
