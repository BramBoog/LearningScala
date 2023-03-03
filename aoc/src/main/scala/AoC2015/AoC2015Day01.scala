object AoC2015Day01 extends App :

  import scala.io.Source

  val instructions: String = Source.fromFile("aoc/src/main/resources/2015/InputDay01.txt")
    .getLines.toList.head

  // Part 1

  println(s"Result part 1: ${instructions.count(_ == '(') - instructions.count(_ == ')')}")


  // Part 2

  def findStepToBasement(instr: List[Char], curPos: Int, i: Int): Int =
    (curPos, instr.head) match
      case (-1, _) => i - 1
      case (_, '(') => findStepToBasement(instr.tail, curPos + 1, i + 1)
      case (_, ')') => findStepToBasement(instr.tail, curPos - 1, i + 1)

  println(s"Result part 2: ${findStepToBasement(instructions.toList, 0, 1)}")
