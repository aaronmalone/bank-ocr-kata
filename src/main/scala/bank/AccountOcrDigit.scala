package bank

/**
 * Class that represents a single, scanned digit from an account number. The
 * digit may or may not be legible (i.e. matching a particular 0-9 digit)
 */
class AccountOcrDigit(val topLine: String, val middleLine: String, val bottomLine: String) {

  require(topLine.length == 3)
  require(middleLine.length == 3)
  require(bottomLine.length == 3)

  def this(string: String) = this(
    topLine = string.grouped(3).toSeq(0),
    middleLine = string.grouped(3).toSeq(1),
    bottomLine = string.grouped(3).toSeq(2))

  /**
   * Represents this account digit as a single character ('?' if illegible)
   */
  val asChar = Digits.mapToCharacter(topLine + middleLine + bottomLine)

  override def equals(that: Any) = {
    that.isInstanceOf[AccountOcrDigit] && {
      val thatDigit = that.asInstanceOf[AccountOcrDigit]
      topLine == thatDigit.topLine &&
        middleLine == thatDigit.middleLine &&
        bottomLine == thatDigit.bottomLine
    }
  }

  override val toString = List(topLine, middleLine, bottomLine).mkString("\n")
}