object AoC2015Day07 extends App:
    import scala.io.Source

    sealed trait Instruction:
        def retName: String
        def evaluate(wiresMap: Map[String, Int]): Option[Int]
        def getWireValueByName(wiresMap: Map[String, Int], wireName: String): Option[Int] =
            wiresMap.get(wireName)

    case class litInput(retName: String, input: Int) extends Instruction:
        def evaluate(wiresMap: Map[String, Int]) =
            Some(input)

    case class singleInput(retName: String, inputName: String, op: Int => Int) extends Instruction:
        def evaluate(wiresMap: Map[String, Int]) =
            getWireValueByName(wiresMap, inputName).map(op)

    case class doubleInput(retName: String, inputName1: String, inputName2: String, op: (Int, Int) => Int) extends Instruction:
        def evaluate(wiresMap: Map[String, Int]) =
            getWireValueByName(wiresMap, inputName1)
            .flatMap(x => getWireValueByName(wiresMap, inputName2).map(op.curried(x)))

    def solve(forWire: String, instructions: List[Instruction], wireOverrides: Map[String, Int] = Map()): Int =
        @annotation.tailrec
        def runInstructions(wires: Map[String, Int], instr: List[Instruction]): Int =
            wires.get(forWire) match
                case Some(a) => a
                case None =>
                    instr.head.evaluate(wires) match
                        case None => runInstructions(wires, instr.tail.appended(instr.head))
                        case Some(x) => runInstructions(wires + (instr.head.retName -> x), instr.tail)

        runInstructions(wireOverrides, instructions.filter(i => !wireOverrides.keySet.contains(i.retName)))

    def parseInstruction(str: String): Instruction =
        str match
            case s"${a} AND ${b} -> ${r}" if a.toIntOption.isDefined => singleInput(r, b, x => 1 & x)
            case s"${a} AND ${b} -> ${r}" => doubleInput(r, a, b, (x,y) => x & y)
            case s"${a} OR ${b} -> ${r}" if a.toIntOption.isDefined => singleInput(r, b, x => 1 | x)
            case s"${a} OR ${b} -> ${r}" => doubleInput(r, a, b, (x,y) => x | y)
            case s"${a} LSHIFT ${b} -> ${r}" => singleInput(r, a, x => x << b.toInt)
            case s"${a} RSHIFT ${b} -> ${r}" => singleInput(r, a, x => x >> b.toInt)
            /* Int is 32 bits, which means taking the bitwise complement of an Int smaller than 32 bits will turn the left few 0's
            (which are there to "fill up" the number of bits to 32) into 1's. This problem works with 16 bits, which means we want to ignore the left 16 bits of an Int.
            To avoid the introduction of (at least) 16 1's we don't want, after taking the bitwise complement, we need to take the bitwise and with a number that has
            16 0's on the left to ignore all the 1's, and 16 1's on the right to let all 16 "actual" bits pass through. This number is 0000FFFF in hexagonal notation 
            (since each position in hexagonal can represent 16 numbers = 4 bits), prefixed by 0x to denote a hexagonal literal. */
            case s"NOT ${a} -> ${r}" => singleInput(r, a, x => (~x) & 0x0000FFFF)
            case s"${a} -> ${r}" if a.toIntOption.isDefined => litInput(r, a.toInt)
            case s"${a} -> ${r}" => singleInput(r, a, identity)
            case _ => sys.error(s"Input not defined: ${str}")

    val instructions: List[Instruction] =
        Source.fromResource("2015/InputDay07.txt")
        .getLines
        .toList
        .map(parseInstruction)


    val answer1: Int = solve("a", instructions)
    println(s"Result part 1: ${answer1}")

    val answer2: Int = solve("a", instructions, Map(("b" -> answer1)))
    println(s"Result part 2: ${answer2}")
