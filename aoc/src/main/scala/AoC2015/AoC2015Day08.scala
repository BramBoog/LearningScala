object AoC2015Day08 extends App:
    import scala.io.Source

    val input: List[String] =
        Source.fromResource("2015/InputDay08.txt")
        .getLines
        .toList

    def countInMemChars(cs: List[Char], acc: Int = 0): Int =
        val hexChars = Set('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'a', 'b', 'c', 'd', 'e', 'f')
        cs match
            case Nil => acc
            case '\\' :: 'x' :: c :: d :: l if (Set(c, d).intersect(hexChars).size == 2) => countInMemChars(l, acc + 1)
            case '\\' :: '\\' :: l => countInMemChars(l, acc + 1)
            case '\\' :: '\"' :: l => countInMemChars(l, acc + 1)
            case '\"' :: l => countInMemChars(l, acc)
            case _ => countInMemChars(cs.tail, acc + 1)

    def unescaped(s: String): String =
        def loop(todo: List[Char], ret: String = ""): String =
            todo match
                case Nil => ret
                case '\\' :: '\\' :: rest => loop(rest, ret + '\\')
                case '\\' :: '\"' :: rest => loop(rest, ret + '\"')
                case '\\' ::  'x' :: h1 :: h2 :: rest => loop(rest, ret + Integer.parseInt(s"$h1$h2", 16).toChar)
                case c1 :: rest => loop(rest, ret + c1)

        loop(s.drop(1).dropRight(1).toList)
    
    val answer1: Int = input.map(s => s.length - countInMemChars(s.toList)).sum
    println(s"Result part 1: ${answer1}")
    val answer1a: Int = input.map(s => s.length - unescaped(s).length).sum
    println(s"Result part 1: ${answer1}")