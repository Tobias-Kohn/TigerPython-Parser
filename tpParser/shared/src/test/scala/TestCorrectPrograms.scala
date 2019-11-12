import org.scalatest._

import tigerpython.parser.Parser

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

  for (fileName <- listAllFiles("correct"))
    test("test program '%s'".format(getFileName(fileName))) {
      val p = new Parser(loadFromCorrectFile(fileName), getPythonVersion(fileName))
      val cs = p.checkSyntax()
      assert(cs.isEmpty)
    }
}
