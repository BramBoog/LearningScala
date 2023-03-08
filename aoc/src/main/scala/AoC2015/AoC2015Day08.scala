object AoC2015Day08 extends App:
    import scala.io.Source

    val input: List[String] =
        Source.fromResource("2015/InputDay08.txt")
        .getLines
        .toList

    def unescaped(s: String): String =
        def loop(todo: List[Char], ret: String = ""): String =
            todo match
                case Nil => ret
                case '\\' :: '\\' :: lst => loop(lst, ret + '\\')
                case '\\' :: '\"' :: lst => loop(lst, ret + '\"')
                case '\"' :: lst => loop(lst, ret)
                case '\\' ::  'x' :: c :: d :: lst => loop(lst, ret + Integer.parseInt(s"$c$d", 16).toChar)
                case c :: lst => loop(lst, ret + c)

        loop(s.drop(1).dropRight(1).toList)

    def escaped(s: String): String =
        def loop(todo: List[Char], ret: String = "\""): String =
            todo match
                case Nil => ret + '\"'
                case '\\' :: lst => loop(lst, ret + "\\\\")
                case '\"' :: lst => loop(lst, ret + "\\\"")
                case c :: lst => loop(lst, ret + c)

        loop(s.toList)            
    
    val answer1: Int = input.map(s => s.length - unescaped(s).length).sum
    println(s"Result part 1: ${answer1}")

    val answer2: Int = input.map(s => escaped(s).length - s.length).sum
    println(s"Result part 2: ${answer2}")
