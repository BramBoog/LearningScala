object AoC2015Day04 extends App:

    import java.security.MessageDigest

    val inputKey: String = "yzbqklnj"

    def findNumberForZeroesHash(n: Int): Int =
        val md: MessageDigest = MessageDigest.getInstance("MD5")

        def loop(i: Int): Int =
            val hash: String = md.digest((inputKey + i.toString).getBytes)
                .map("%02X".format(_))
                .mkString

            if (hash.substring(0, n) == "0" * n) i
            else loop(i + 1)

        loop(1)

    // Part 1

    val answer1: Int = findNumberForZeroesHash(5)

    println(s"Result part 1: ${answer1}")

    // Part 2

    val answer2: Int = findNumberForZeroesHash(6)

    println(s"Result part 2: ${answer2}")
