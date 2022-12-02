object AoC2021Day16 extends App:
    import scala.io.Source

    def bitsToInt(bitString: String): Int =
        Integer.parseInt(bitString, 2)

    def bitsToBigInt(bitString: String): BigInt =
        BigInt(bitString, 2)

    def hexToBin(hexString: String): String =
        val bits = BigInt(hexString, 16).toString(2)
        "0" * ((4 - bits.length % 4) % 4) + bits
    

    case class BitGroup(bitString: String):
        val lastGroup: Boolean = bitString.head == '0'
        val bodyBits: String = bitString.substring(1,5)

    sealed trait Packet:
        def version: Int
        def typeID: Int
        def length: Int
        def evaluate: BigInt

    case class LitVal(version: Int, typeID: Int, length: Int, value: BigInt) extends Packet:
        def evaluate: BigInt = value

    case class Operator(version: Int, typeID: Int, length: Int, lengthTypeID: Int, subPackets: List[Packet]) extends Packet:
        val op: (List[BigInt] => BigInt) =
            typeID match
                case 0 => _.sum
                case 1 => l => l.product
                case 2 => _.min
                case 3 => _.max
                case 5 => l => if (l(0) > l(1)) 1 else 0
                case 6 => l => if (l(0) < l(1)) 1 else 0
                case 7 => l => if (l(0) == l(1)) 1 else 0
                case _ => sys.error(s"Undefined typeID: ${typeID}")
            
        def evaluate: BigInt =
            op(subPackets.map(_.evaluate))


    def parsePacket(inputString: String): (String, Packet) =
        def parseLitVal(bitString: String): (String, LitVal) =
            val version = bitsToInt(bitString.substring(0,3))
            val typeID = bitsToInt(bitString.substring(3,6))

            def parseGroups(str: String, groups: List[BitGroup]): List[BitGroup] =
                val curGroup = BitGroup(str.substring(0,5))
                if (curGroup.lastGroup) groups.appended(curGroup)
                else parseGroups(str.substring(5), groups.appended(curGroup))
            
            val parsedGroups = parseGroups(bitString.substring(6), List())
            val packetLength = parsedGroups.length * 5 + 6
            (bitString.substring(packetLength), LitVal(version, typeID, packetLength, bitsToBigInt(parsedGroups.map(_.bodyBits).mkString)))

        def parseOperator(bitString: String): (String, Operator) =
            val version = bitsToInt(bitString.substring(0,3))
            val typeID = bitsToInt(bitString.substring(3,6))
            val lengthTypeID = bitsToInt(bitString.substring(6,7))

            def parseSubPackets(str: String, subPackets: List[Packet], doneAt: Int, evaluateProgress: (List[Packet]) => Int): List[Packet] =
                if (evaluateProgress(subPackets) == doneAt) subPackets
                else
                    val (remString, curPacket) = parsePacket(str)
                    parseSubPackets(remString, subPackets.appended(curPacket), doneAt, evaluateProgress)

            if (lengthTypeID == 0)
                val parsedSubPackets = parseSubPackets(bitString.substring(22), List(), bitsToInt(bitString.substring(7, 22)), (l: List[Packet]) => l.map(_.length).fold(0)(_+_))
                val packetLength = parsedSubPackets.map(_.length).fold(0)(_+_) + 7 + 15
                (bitString.substring(packetLength), Operator(version, typeID, packetLength, lengthTypeID, parsedSubPackets))
            else if (lengthTypeID == 1)
                val parsedSubPackets = parseSubPackets(bitString.substring(18), List(), bitsToInt(bitString.substring(7, 18)), (l: List[Packet]) => l.length)
                val packetLength = parsedSubPackets.map(_.length).fold(0)(_+_) + 7 + 11
                (bitString.substring(packetLength), Operator(version, typeID, packetLength, lengthTypeID, parsedSubPackets))
            else sys.error(s"Unkown lengthTypeID: ${lengthTypeID}")

        bitsToInt(inputString.substring(3,6)) match
            case 4 => parseLitVal(inputString)
            case _ => parseOperator(inputString)


    val transmission: String = 
        hexToBin(Source.fromResource("2021/InputDay16.txt").getLines.toList.head)

    val fullPacket: Packet = parsePacket(transmission)._2

    // Part 1
    def addVersionNumbers(p: Packet): Int =
        p match
            case LitVal(version, typeID, length, value) => version
            case Operator(version, typeID, length, lengthTypeID, subPackets) => version + subPackets.map(addVersionNumbers).sum
    
    val answer1: Int = addVersionNumbers(fullPacket)
    println(s"Result part 1: ${answer1}")

    // Part 2
    val answer2: BigInt = fullPacket.evaluate
    println(s"Result part 2: ${answer2}")
