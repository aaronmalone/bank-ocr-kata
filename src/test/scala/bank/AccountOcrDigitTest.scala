package bank

import org.scalatest.WordSpec

class AccountOcrDigitTest extends WordSpec {

  def doTest(str: String, digit: Char) {
    assert(new AccountOcrDigit(str).asChar === digit)
  }

  "An AccountOcrDigit" should {
    "provide a method" that {
      "returns a single digit character" in {
        doTest(Digits.Zero, '0')
        doTest(Digits.One, '1')
        doTest(Digits.Two, '2')
        doTest(Digits.Three, '3')
        doTest(Digits.Four, '4')
        doTest(Digits.Five, '5')
        doTest(Digits.Six, '6')
        doTest(Digits.Seven, '7')
        doTest(Digits.Eight, '8')
        doTest(Digits.Nine, '9')
      }

      "returns a '?' character for illegible account digits" in {
        val illegibleFive =
          " _ " +
            " _ " +
            " _|"
        doTest(illegibleFive, '?')

        val illegibleNine =
          " _ " +
            "|_|" +
            " _ "
        doTest(illegibleNine, '?')
      }

      "correct compares equality (an equals method)" in {
        val digitStrings = List(Digits.Six, Digits.One, Digits.Four)
        for (
          digitStrA <- digitStrings;
          digitStrB <- digitStrings;
          digitA = new AccountOcrDigit(digitStrA);
          digitB = new AccountOcrDigit(digitStrB)
        ) assert(digitStrA.equals(digitStrB) === digitA.equals(digitB))
      }
    }
  }

}