object AoC2015Day05 extends App:

    import scala.io.Source

    val strings: List[String] = 
        Source.fromResource("2015/InputDay05.txt")
        .getLines
        .toList

    case class Word(str: String):
        val vowels: Set[Char] = Set('a', 'e', 'i', 'o', 'u')

        val prohibitedStrings: Set[String] = Set("ab", "cd", "pq", "xy")

        val pairs: Iterator[String] = str.sliding(2)

        def threeVowels(): Boolean =
            str.filter(char => vowels.contains(char)).length >= 3
        
        def twoLettersInRow(): Boolean =
            val pairs1: Seq[String] =
                for i <- 0 until str.length - 1 if str(i) == str(i+1)
                yield str(i).toString.appended(str(i+1))

            pairs1.length > 0

        def noProhibitedSubstrings(): Boolean =
            val subs: Set[String] =
                for sub <- prohibitedStrings if str.contains(sub)
                yield sub
                
            subs.size == 0

        def isNice1(): Boolean =
            threeVowels() & twoLettersInRow() & noProhibitedSubstrings()

        def twoLettersInRowAlt(): Boolean =
            pairs.map(s => s(0) == s(1)).contains(true)

        def noProhibitedSubstringsAlt(): Boolean =
            println(pairs.toList);
            ! pairs.map(s => prohibitedStrings.contains(s)).contains(true)

        def isNice1Alt(): Boolean =
            threeVowels() & twoLettersInRowAlt() & noProhibitedSubstringsAlt()

        def recurringPair(): Boolean =
            ???

    //val answer1: Int = strings.map(Word(_).isNice1()).count(_ == true)

    //println(s"Result part 1: ${answer1}")
    val word = Word(strings(123))

    //println(strings.map(s => (Word(s), Word(s).isNice1Alt())).diff(strings.map(s => (Word(s), Word(s).isNice1()))))
    println(word.pairs.toList)
    println(! word.pairs.map(s => word.prohibitedStrings.contains(s)).contains(true))
    println(word.pairs.toList)
    println(word.noProhibitedSubstringsAlt())
