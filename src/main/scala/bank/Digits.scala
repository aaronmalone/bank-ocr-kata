package bank

object Digits {

  /* if you use ScalaIDE's auto-format functionality on this file, it will screw
   * up the formatting that makes the account digits readable, so don't do that!
   */

  val One =
      "   " +
      "  |" +
      "  |"

  val Two =
      " _ " +
      " _|" +
      "|_ "

  val Three =
      " _ " +
      " _|" +
      " _|"

  val Four =
      "   " +
      "|_|" +
      "  |"

  val Five =
      " _ " +
      "|_ " +
      " _|"

  val Six =
      " _ " +
      "|_ " +
      "|_|"

  val Seven =
      " _ " +
      "  |" +
      "  |"

  val Eight =
      " _ " +
      "|_|" +
      "|_|"

  val Nine =
      " _ " +
      "|_|" +
      " _|"

  val Zero =
      " _ " +
      "| |" +
      "|_|"

  def mapToCharacter(str: String): Char = {
    assert(str.length == 9, s"length of '$str' is not 9")
    str match {
    case Zero => '0'
      case One => '1'
      case Two => '2'
      case Three => '3'
      case Four => '4'
      case Five => '5'
      case Six => '6'
      case Seven => '7'
      case Eight => '8'
      case Nine => '9'
      case _ => '?'
    }
  }

  def mapToDigitString(char: Char): String = {
    char match {
      case '0' => Zero
      case '1' => One
      case '2' => Two
      case '3' => Three
      case '4' => Four
      case '5' => Five
      case '6' => Six
      case '7' => Seven
      case '8' => Eight
      case '9' => Nine
      case '?' => " " * 9
    }
  }
}