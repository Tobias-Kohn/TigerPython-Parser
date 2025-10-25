import org.scalatest._
import tigerpython.parser.Parser
import tigerpython.parser.errors.ErrorHandler

/**
  * Tests correct programs without errors.
  *
  * The parser is supposed not to find any errors or mistakes in the files.
  *
  * Note that the parser slightly deviates from Python's usual standards and will mark suspicious code as error even
  * when the code is perfectly legal as standard Python.
  *
  * @author Tobias Kohn
  *
  * Created: 08/11/2019
  * Updated: 09/11/2019
  */
class TestCorrectPrograms extends FunSuite {

  /*
   * Files without errors are just plain Python files.
   */
  private def loadFromCorrectFile(fileName: String): String = {
    val src = scala.io.Source.fromFile(fileName)
    src.getLines().mkString("\n")
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

  /**
   * Given the program code and a position, this will extract the 'offensive' line and highlight the
   * position of the problem.
   */
  private def extractSliceFromText(text: String, pos: Int): String =
    if (0 <= pos && pos < text.length) {
      var i = pos
      var j = pos
      while (i > 0 && text(i-1) >= ' ')
        i -= 1
      while (j < text.length && text(j) >= ' ')
        j += 1
      text.substring(i, j) + "\n" + (" " * (pos - i)) + "^\n"
    } else
      null

  // We want the additional information on where the error occurred
  ErrorHandler.WANT_STACK_TRACE = true

  for (fileName <- listAllFiles("correct"))
    test("test program '%s'".format(getFileName(fileName))) {
      val p = new Parser(loadFromCorrectFile(fileName), getPythonVersion(fileName))
      p.repeatStatement = true
      val cs = p.checkSyntax()

      // In case of an error, we print some extended information to help debugging
      if (cs.nonEmpty) {
        print(cs.get.toString + "\n" + extractSliceFromText(loadFromCorrectFile(fileName), cs.get.position))
        val ste = cs.get.stackTraceElement
        if (ste != null)
          println("%s [%d]: %s.%s".format(ste.getFileName, ste.getLineNumber, ste.getClassName, ste.getMethodName))
      }
      assert(cs.isEmpty)
    }
}
