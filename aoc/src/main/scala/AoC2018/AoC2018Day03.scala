object AoC2018Day03 extends App :

  import scala.io.Source
  import scala.util.matching.Regex

  val input: List[String] = Source.fromFile("aoc/src/main/resources/2018/InputDay03.txt").getLines.toList

  val keyPattern: Regex = """.(\d+)\s@""".r

  //val squares = input.map(str.)

  //Part 1


  val fabricArray: Array[Array[List[Int]]] = Array.fill(1000)(Array.fill(1000)(List()))

  //println(keyPattern.findFirstMatchIn(input(0)))
  println(keyPattern.findFirstMatchIn(input(1)))
