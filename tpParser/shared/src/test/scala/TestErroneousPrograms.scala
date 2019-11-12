import org.scalatest._

import tigerpython.parser.Parser

/**
  * Tests programs with errors.
  *
  * Each file tested is supposed to contain the expected error code and the line number of the error.  The parser is
  * then supposed to output the respective line and error code.
  *
  * Note that the parser slightly deviates from Python's usual standards and will mark suspicious code as error even
  * when the code is perfectly legal as standard Python.
  *
  * @author Tobias Kohn
  *
  * Created: 08/11/2019
  * Updated: 09/11/2019
  */
class TestErroneousPrograms extends FunSuite {

  /*
   * In order to test programs with errors, we expect a format where the first and second line are comments.  The
   * first line just contains a number indicating the line number that is incorrect.  The second line contains the
   * error message to be displayed in English.
   */
  private def loadFromErrorFile(fileName: String): (Int, String, String) = {
    val src = scala.io.Source.fromFile(fileName)
    val lines = src.getLines().toArray
    if (lines.length > 2) {
      val line_no = lines(0).dropWhile(!_.isDigit).takeWhile(_.isDigit).toInt
      val error_msg = lines(1).drop(1).trim
      val text = lines.drop(2).mkString("\n")
      (line_no, error_msg, text)
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

  private def getPythonVersion(fileName: String): Int = {
    val f = fileName.toLowerCase
    if (f.endsWith(".py2") || f.endsWith(".py2.txt") || f.endsWith(".2.py") || f.endsWith(".2.txt"))
      2
    else
      3
  }

  for (fileName <- listAllFiles("erroneous"))
    test("test program '%s'".format(getFileName(fileName))) {
      val (line_no, error_msg, text) = loadFromErrorFile(fileName)
      val p = new Parser(text, getPythonVersion(fileName))
      p.rejectDeadCode = true
      p.strictCode = true
      val cs = p.checkSyntax()
      assert(cs.isDefined)
      // `line_no` expects the first line to be `1` and not `0`
      assert(cs.get.line+1 == line_no)
      assert(cs.get.errorCode.toString == error_msg)
    }
}
