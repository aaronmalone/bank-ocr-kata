package bank

import org.scalatest.WordSpec

class DigitsTest extends WordSpec {

  "Digits" should {
    "provide a method that maps nine-character " +
      "strings to single-digit characters, if the strings correctly \"print-out\"" +
      " an account digit" in {
        assert(Digits.mapToCharacter(Digits.Six) === '6')
        assert(Digits.mapToCharacter(Digits.One) === '1')
        assert(Digits.mapToCharacter(Digits.Four) === '4')
      }

    "provide a method that maps nine-character " +
      "strings to a '?' character when the string does not correctly \"print-out\"" +
      "an account digit" in {

        val incorrectFive =
            " _ " +
            " _ " +
            " _|"
        assert(Digits.mapToCharacter(incorrectFive) === '?')

        val incorrectNine =
            " _ " +
            "|_|" +
            " _ "
        assert(Digits.mapToCharacter(incorrectNine) === '?')
      }

    "provide a method that maps single character digits " +
      "to nine-character account digit strings" in {
    	def doTest(digit: Char, expectedOutput: String) {
    	  assert(Digits.mapToDigitString(digit) === expectedOutput)
    	}
    	doTest('6', Digits.Six)
    	doTest('1', Digits.One)
    	doTest('4', Digits.Four)
      }
  }
}