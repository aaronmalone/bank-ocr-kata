package bank

import scala.io.Source

object BankOcrIO {
  
  def readOcrInputFile(filename: String) = {
    readOcrInputSource(Source.fromFile(filename))
  }
  
  def readOcrInputSource(source: Source) = {
    readOcrInputLines(source.getLines)
  }
  
  def readOcrInputLines(linesIterator: Iterator[String]): Seq[AccountNumber] = {
    val lineSeq = linesIterator.toIndexedSeq
    requireFourthLineJustSpaces(lineSeq.grouped(4))
    for(fourLines <- lineSeq.grouped(4).toSeq) yield AccountNumber(fourLines)
  }

  private def requireFourthLineJustSpaces(groupedIterator: Iterator[Seq[String]]) {
    def fourthLineJustSpaces(seqStr: Seq[String]) = seqStr(3).forall(_ == ' ')
    def messageGenerator = (ss: Seq[String]) => "Non-blank fourth line: " + ss(3)
    requireForAll(groupedIterator, fourthLineJustSpaces, messageGenerator)
  }
  
  def formatAccountNumber(acctNum: AccountNumber): String = {
    val accountString = acctNum.accountString
    if(accountString.contains("?")) {
      accountString + " ILL"
    } else if(!acctNum.validChecksum) {
      accountString + " ERR"
    } else {
      accountString
    }
  }
}