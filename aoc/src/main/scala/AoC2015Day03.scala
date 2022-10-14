object AoC2015Day03 extends App:

    import scala.io.Source

    val directions: String =
        Source.fromResource("2015/InputDay03.txt")
        .getLines
        .toList.head

    case class House(x: Int, y: Int)

    println(Set(House(1,2), House(1,3), House(1,2)))
