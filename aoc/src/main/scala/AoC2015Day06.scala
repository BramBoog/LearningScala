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


    def applyInstructions[A](lightsArray: List[List[A]], instrHandler: (A, Instruction) => A): List[List[A]] = 
        (for {
            (row, i) <- lightsArray.zipWithIndex
        }
        yield
            row.zipWithIndex
            .map((el, j) => 
                instructions
                .filter(instr => instr.bottomLeftX <= j && j <= instr.topRightX && instr.bottomLeftY <= i && i <= instr.topRightY)
                .foldLeft(el)(instrHandler)
            )
        )
    
    // Part 1

    def takeLightInstruction1(cur: Boolean, instr: Instruction): Boolean =
        instr.instructionType match
            case "on" => true
            case "off" => false
            case "toggle" => !cur

    val answer1: Int = applyInstructions(List.tabulate(1000, 1000)((_,_)=>false), takeLightInstruction1).flatten.count(_==true)

    println(s"Result part 1: ${answer1}")

    // Part 2

    def takeLightInstruction2(cur: Int, instr: Instruction): Int =
        (instr.instructionType, cur) match
            case ("on", light) => light + 1
            case ("off", light) => if (light > 0) light - 1 else 0
            case ("toggle", light) => light + 2

    val answer2: Int = applyInstructions(List.tabulate(1000, 1000)((_,_)=>0), takeLightInstruction2).flatten.sum

    println(s"Result part 1: ${answer2}")
