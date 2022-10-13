import org.scalatest.funsuite.AnyFunSuite

class Solutions2015 extends AnyFunSuite:

  test("AoC2015Day02") {
    assertResult( 1606483)(actual = AoC2015Day02.answer1)
    assertResult( 3842356)(actual = AoC2015Day02.answer2)
  }