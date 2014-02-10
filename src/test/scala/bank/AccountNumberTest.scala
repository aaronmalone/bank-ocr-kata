package bank

import org.scalatest.WordSpec

class AccountNumberTest extends WordSpec {

  def interceptBadAcctInput(acctLines: List[String]): IllegalArgumentException = {
    intercept[IllegalArgumentException] {
      AccountNumber(acctLines)
    }
  }
  
  "An AccountNumber," when {

    "constructed with less than four lines," should {
      "throw an IllegalArgumentException" in {
        val justThreeLines = List(
          "  | _| _||_||_ |_   ||_||_|",
          "  ||_  _|  | _||_|  ||_| _|",
          "                           ")
        interceptBadAcctInput(justThreeLines)
      }
    }

    "constructed with more than four lines," should {
      "throw an IllegalArgumentException" in {
        val fiveLines = List(
          "    _  _     _  _  _  _  _ ",
          "  | _| _||_||_ |_   ||_||_|",
          "  ||_  _|  | _||_|  ||_| _|",
          "  ||_  _|  | _||_|  ||_| _|",
          "                           ")
        interceptBadAcctInput(fiveLines)
      }
    }

    "constructed with an illegal character," should {
      "throw an IllegalArgumentException has the illegal character at the end of the message" in {
        val illegalAcctNumLines = List(
          "    _  _     _  _  _  _  _ ",
          "C | _| _||_||_ |_   ||_||_|",
          "  ||_  _|  | _||_|  ||_| _|",
          "                           ")

        val interceptedException = interceptBadAcctInput(illegalAcctNumLines)
        val excMessage = interceptedException.getMessage
        assert(excMessage.last === 'C')
      }
    }

    "constructed with lines of incorrect length," should {
      "throw an IllegalArgumentException" in {
        val oneLineShort = List(
          "    _  _     _  _  _  _  _ ",
          "  | _| _||_||_ |_   ||_||_",
          "  ||_  _|  | _||_|  ||_| _|",
          "                           ")
        interceptBadAcctInput(oneLineShort)
        val oneLineLong = List(
          "    _  _     _  _  _  _  _ ",
          "  | _| _||_||_ |_   ||_||_||",
          "  ||_  _|  | _||_|  ||_| _|",
          "                           ")
        interceptBadAcctInput(oneLineLong)
      }
    }
  }

  "An AccountNumber" should {
    "return the correct account number sequence" in {

      val linesFor123456789 = List(
        "    _  _     _  _  _  _  _ ",
        "  | _| _||_||_ |_   ||_||_|",
        "  ||_  _|  | _||_|  ||_| _|",
        "                           ")

      val linesFor000000000 = List(
        " _  _  _  _  _  _  _  _  _ ",
        "| || || || || || || || || |",
        "|_||_||_||_||_||_||_||_||_|",
        "                           ")

      val linesFor000000051 = List(
        " _  _  _  _  _  _  _  _    ",
        "| || || || || || || ||_   |",
        "|_||_||_||_||_||_||_| _|  |",
        "                           ")

      val linesFor49006771_ = List(
        "    _  _  _  _  _  _     _ ",
        "|_||_|| || ||_   |  |  | _ ",
        "  | _||_||_||_|  |  |  | _|",
        "                           ")
        
      assert(AccountNumber(linesFor123456789).accountString === "123456789")
      assert(AccountNumber(linesFor000000000).accountString === "000000000")
      assert(AccountNumber(linesFor000000051).accountString === "000000051")
      assert(AccountNumber(linesFor49006771_).accountString === "49006771?")
    }
    
    "provide a method that computes a checksum for valid input" in {
      def doTest(str: String, valid: Boolean) {
        if(valid) 
          assert(AccountNumber(str).checksum === 0)
        else 
          assert(AccountNumber(str).checksum != 0)
      }
      
      doTest("457508000", valid = true)
      doTest("888886888", valid = true)
      doTest("888888880", valid = true)
      doTest("490867715", valid = true)
      
      doTest("333333333", valid = false)
      doTest("555555555", valid = false)
      doTest("888888888", valid = false)
      doTest("490067715", valid = false)
    }
  }

  "AccountNumber" should {
    "provide a method that returns an AccountOcrDigit for an index in the account number input" in {
      val lines = List(
        "    _  _     _  _  _  _  _ ",
        "  | _| _||_||_ |_   ||_||_|",
        "  ||_  _|  | _||_|  ||_| _|",
        "                           ")
      def test(digitStr: String, digitIndex: Int) {
        assert(new AccountOcrDigit(digitStr) == AccountNumber.getAccountOcrDigit(lines, digitIndex))
      }

      /* NOTE: index is zero-based, hence the minus 1 */
      test(Digits.Six, 6 - 1)
      test(Digits.One, 1 - 1)
      test(Digits.Four, 4 - 1)
    }
    
  }
}