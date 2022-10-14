object AoC2015Day03 extends App:

    import scala.io.Source

    val directions: String =
        Source.fromResource("2015/InputDay03.txt")
        .getLines
        .toList.head

    case class House(x: Int, y: Int)

    // Part 1

    def findVisitedHouses(curHouse: House, visitedHouses: Set[House], directionsStr: String): Set[House] =
        if (directionsStr.isEmpty) visitedHouses.incl(curHouse)
        else
            val newHouse = directionsStr.head match 
                case '>' => House(curHouse.x + 1, curHouse.y)
                case '<' => House(curHouse.x - 1, curHouse.y)
                case '^' => House(curHouse.x, curHouse.y + 1)
                case 'v' => House(curHouse.x, curHouse.y - 1)
            
            findVisitedHouses(newHouse, visitedHouses.incl(curHouse), directionsStr.tail)

    val answer1: Int = findVisitedHouses(House(0, 0), Set(), directions).size
    
    println(s"Result part 1: ${answer1}")

    // Part 2

    val directionsSanta: String =
        directions.zipWithIndex.filter((_, i) => i % 2 == 0).map((char, _) => char).foldLeft("")((str, char) => str.appended(char))
    val visitedHousesSanta: Set[House] = findVisitedHouses(House(0, 0), Set(), directionsSanta)
    
    val directionsRoboSanta: String = 
        directions.zipWithIndex.filter((_, i) => i % 2 == 1).map((char, _) => char).foldLeft("")((str, char) => str.appended(char))
    val visitedHousesRoboSanta: Set[House] = findVisitedHouses(House(0, 0), Set(), directionsRoboSanta)

    val answer2: Int = visitedHousesSanta.union(visitedHousesRoboSanta).size

    println(s"Result part 2: ${answer2}")
    