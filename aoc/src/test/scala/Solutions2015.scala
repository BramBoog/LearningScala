import org.scalatest.funsuite.AnyFunSuite

class Solutions2015 extends AnyFunSuite:

  test("AoC2015Day02") {
    assertResult(1606483)(actual = AoC2015Day02.answer1)
    assertResult(3842356)(actual = AoC2015Day02.answer2)
  }

  test("AoC2015Day03") {
    assertResult(2565)(actual = AoC2015Day03.answer1)
    assertResult(2639)(actual = AoC2015Day03.answer2)
  }

  // test("AoC2015Day04") {
  //   assertResult(282749)(actual = AoC2015Day04.answer1)
  //   assertResult(9962624)(actual = AoC2015Day04.answer2)
  // }

  test("AoC2015Day05") {
    assertResult(255)(actual = AoC2015Day05.answer1)
    assertResult(55)(actual = AoC2015Day05.answer2)
  }

  test("AoC2015Day06") {
    assertResult(377891)(actual = AoC2015Day06.answer1)
    assertResult(14110788)(actual = AoC2015Day06.answer2)
  }
