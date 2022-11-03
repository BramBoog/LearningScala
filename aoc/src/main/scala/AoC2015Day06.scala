object AoC2015Day06 extends App:
    import scala.io.Source
    import scala.util.matching.Regex

    case class Instruction(instructionType: String, bottomLeftX: Int, bottomLeftY: Int, topRightX: Int, topRightY: Int)

    val instructions: List[Instruction] = 
        Source.fromResource("2015/InputDay06.txt")
        .getLines
        .toList
        .map(
            str => str match
                // turn on/turn off
                case s"turn ${instructionType} ${bottomLeftX},${bottomLeftY} through ${topRightX},${topRightY}"
                    => Instruction(instructionType, bottomLeftX.toInt, bottomLeftY.toInt, topRightX.toInt, topRightY.toInt)
                // toggle
                case s"${instructionType} ${bottomLeftX},${bottomLeftY} through ${topRightX},${topRightY}"
                    => Instruction(instructionType, bottomLeftX.toInt, bottomLeftY.toInt, topRightX.toInt, topRightY.toInt)
                case _ => throw new IllegalArgumentException
        )        

    val lightsArrayInit: List[List[Int]] = List.fill(1000)(List.fill(1000)(0))

    def computeTotalLights(lightsArray: List[List[Int]], instrHandler: (Int, Instruction) => Int): Int = 
        (for {
            (row, i) <- lightsArray.zipWithIndex
        }
        yield
            row.zipWithIndex
            .map((el, j) => 
                instructions
                .filter(instr => (instr.bottomLeftX <= j) && (j <= instr.topRightX) && (instr.bottomLeftY <= i) && (i <= instr.topRightY))
                .foldLeft(el)(instrHandler)
            )
        ).flatten.sum
    
    // Part 1

    def takeLightInstruction1(cur: Int, instr: Instruction): Int =
        (instr.instructionType, cur) match
            case ("on", _) => 1
            case ("off", _) => 0
            case ("toggle", 0) => 1
            case ("toggle", 1) => 0

    val answer1: Int = computeTotalLights(lightsArrayInit, takeLightInstruction1)

    println(s"Result part 1: ${answer1}")

    // Part 2

    def takeLightInstruction2(cur: Int, instr: Instruction): Int =
        (instr.instructionType, cur) match
            case ("on", light) => light + 1
            case ("off", light) => if (light > 0) light - 1 else 0
            case ("toggle", light) => light + 2

    val answer2: Int = computeTotalLights(lightsArrayInit, takeLightInstruction2)

    println(s"Result part 1: ${answer2}")
