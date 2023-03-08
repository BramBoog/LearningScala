object Aoc2018Day02 extends App :

  import scala.io.Source

  val ids: List[String] = Source.fromFile("aoc/src/main/resources/2018/InputDay02.txt").getLines.toList

  //Part 1

  def countDupes(wordsList: List[String]): Int =

    def letterCounts(word: String): List[Int] =
      word
        .groupBy(identity)
        .map((_, v) => v.length)
        .toList

    val wordsListCounts: List[List[Int]] =
      wordsList
        .map(letterCounts)
        .filter(l => l.contains(2) || l.contains(3))

    val countTwo: Int = wordsListCounts.count(_ contains 2)
    val countThree: Int = wordsListCounts.count(_ contains 3)

    countTwo * countThree

  println(s"Result part 1: ${countDupes(ids)}")

  //Part 2

  def findSimilarIds(wordsList: List[String], wordsListLoop: List[String]): (String, String) =
    val word: String = wordsList.head
    if (wordsListLoop.isEmpty) findSimilarIds(wordsList.tail, ids)
    else if (
      word.diff(wordsListLoop.head).length == 1
        &&
        word.zip(wordsListLoop.head).count((x, y) => x != y) == 1
    ) (word, wordsListLoop.head)
    else findSimilarIds(wordsList, wordsListLoop.tail)

  val (similarId1, similarId2) = findSimilarIds(ids, ids)
  val sharedCharsAsString =
    similarId1
      .zip(similarId2)
      .filter((x, y) => x == y)
      .map((a, _) => a)
      .foldLeft("")((x, y) => x.toString + y.toString)

  println(s"Result part 2: ${sharedCharsAsString}")
