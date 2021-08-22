import org.scalatest._

import tigerpython.utilities.completer._

/**
  *
  *
  * @author Tobias Kohn
  *
  * Created: 22.08.2021
  * Updated: 22.08.2021
  */
class TestCompleter extends FunSuite {

  /*
   * Files without errors are just plain Python files.
   */
  /*
   * In order to test programs with errors, we expect a format where the first and second line are comments.  The
   * first line just contains a number indicating the line number that is incorrect.  The second line contains the
   * error message to be displayed in English.
   */
  private def loadFromCompleterFile(fileName: String): (Int, String, String) = {
    val src = scala.io.Source.fromFile(fileName)
    val lines = src.getLines().toArray
    if (lines.length > 2) {
      val pos = lines(0).dropWhile(!_.isDigit).takeWhile(_.isDigit).toInt
      val expected_result = lines(1).drop(1).trim
      val text = lines.drop(2).mkString("\n")
      (pos, expected_result, text)
    } else
      (-1, "", "")
  }

  private def listAllFiles(subDir: String): Array[String] = {
    val f = new java.io.File("./tpParser/shared/src/test/programs/%s/".format(subDir))
    val fl = f.listFiles()
    if (fl != null)
      fl.map(_.getAbsolutePath.replace("/./", "/"))
    else
      Array()
  }

  private def getFileName(fileName: String): String = {
    val f = new java.io.File(fileName)
    val n = f.getName
    if (n.contains('.'))
      n.take(n.lastIndexOf('.'))
    else
      n
  }

  for (fileName <- listAllFiles("completer"))
    test("test program '%s'".format(getFileName(fileName))) {
      val (pos, expected_result, text) = loadFromCompleterFile(fileName)
      val completer = new Completer(fileName, text, pos)
      val suggestions = completer.getNameFilter.getNameList("").mkString(";")
      assert(suggestions == expected_result)
    }
}
