package bank

import scala.collection.immutable.Stream.Cons

/**
 * Class that represents a scanned account number.
 */
class AccountNumber(digits: Seq[AccountOcrDigit]) {

  require(digits.length == 9)

  /**
   * A nine-character, single-line string representing this account number
   */
  val accountString: String = digits.map(_.asChar).mkString

  override val toString = "AccountNumber(\"" + accountString + "\")"

  /**
   * Calculates the checksum for this scanned account number.
   * <br>
   * Returns -1 if the account number contains illegible digits, otherwise
   * returns the computed checksum.
   */
  val checksum: Int = {
    val digitChars = digits.map(_.asChar)
    if (digitChars.contains('?)) {
      -1
    } else {
      calculateChecksumForLegibleDigits(digitChars.map(_.asDigit))
    }
  }
  
  /**
   * Returns true if the checksum is valid, false otherwise
   */
  val validChecksum: Boolean = checksum == 0

  def calculateChecksumForLegibleDigits(ds: Seq[Int]): Int = {
    def recursiveImpl(multiplier: Int, digits: List[Int]): Int = {
      digits match {
        case Nil => 0
        case head :: tail => multiplier * head + recursiveImpl(multiplier - 1, tail)
      }
    }
    recursiveImpl(9, ds.toList) % 11
  }
}

/*
 * The AccountNumber companion object contains methods for constructing 
 * AccountNumber instances
 */
object AccountNumber {

  def apply(charSeq: String): AccountNumber = {
    require(charSeq.length == 9, "length was not 9: " + charSeq)
    val stringSeq = charSeq.map(Digits.mapToDigitString(_))
    val accountOcrDigitSeq = stringSeq.map(new AccountOcrDigit(_))
    new AccountNumber(accountOcrDigitSeq)
  }

  def apply(acctLines: Seq[String]): AccountNumber = {
    validateInputLines(acctLines)
    new AccountNumber(getAccountDigits(acctLines))
  }

  private def validateInputLines(inputLines: Seq[String]) {
    require(inputLines.size == 4, "Account number was not four lines.")
    requireForAll[String](inputLines, _.length == 27, line => "Line length was not 27: " + line)
    checkLegalChars(inputLines)
  }

  private def checkLegalChars(lines: Seq[String]) {
    val legalChars = List(' ', '|', '_')
    val accountNumberChars = lines.mkString
    requireForAll[Char](accountNumberChars, legalChars.contains(_), (ch) => "Illegal character: " + ch)
  }

  private def getAccountDigits(acctLines: Seq[String]): Seq[AccountOcrDigit] = 
    for (digitIndex <- 0 to 8) yield getAccountOcrDigit(acctLines, digitIndex)
  

  /* elevated visibility for testing */
  def getAccountOcrDigit(acctLines: Seq[String], digitIndex: Int): AccountOcrDigit = {
    val CharsPerDigit = 3
    val digitLines = acctLines.map {
      _.grouped(CharsPerDigit).toSeq(digitIndex)
    }
    new AccountOcrDigit(topLine = digitLines(0), middleLine = digitLines(1), bottomLine = digitLines(2))
  }
}