package bank

import org.scalatest.FunSuite
import scala.io.Source
import java.io.InputStream

class BankOcrIoTest extends FunSuite {
  
  def inputSource: Source = {
    val in: InputStream = getClass().getClassLoader().getResourceAsStream("bank_ocr.txt")
    Source.fromInputStream(in)
  }
  
  test("BankOcrIO should read an input source and return AccountNumber instances") {
    val accountNumbers = BankOcrIO.readOcrInputSource(inputSource)
    /* there happen to be 26 accounts in the file */
    assert(accountNumbers.size === 26)
  }
  
  test("BankOcrIO should provide a method that takes an AccountNumber and returns " + 
      "a single line with \"ERR\" or \"ILL\" as appropriate") {
    def doTest(input: String, formattedOutput: String) {
      assert(BankOcrIO.formatAccountNumber(AccountNumber(input)) === formattedOutput)
    }
    doTest("457508000", "457508000")
    doTest("664371495", "664371495 ERR")
    doTest("888888888", "888888888 ERR")
    doTest("555555555", "555555555 ERR")
    doTest("86110??36", "86110??36 ILL")
    doTest("457508?00", "457508?00 ILL")
  }
  
  test("BankOcrIO should throw an exception if a fourth line is not blank"){
    val lines = inputSource.getLines.toList
    val plusAnExtraLine = (" " * 27) :: lines 
    val asString = plusAnExtraLine.mkString("\n")
    val stringSource = Source.fromString(asString)
    intercept[IllegalArgumentException] {
      BankOcrIO.readOcrInputSource(stringSource)
    }
  }
}