import org.scalatest._

import tigerpython.inputenc
import tigerpython.parser.Parser

/**
  * Tests the string-translator for less common punctuation.
  *
  * @author Tobias Kohn
  *
  * Created: 17/12/2019
  * Updated: 18/12/2019
  */
class TestTranslator extends FunSuite {

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

  for (fileName <- listAllFiles("correct"))
    test("test translation '%s'".format(getFileName(fileName))) {
      val txt = loadFromCorrectFile(fileName)
      val transl = inputenc.StringTranslator.translate(txt)
      assert(txt == transl)
    }

  for (fileName <- listAllFiles("unicode"))
    test("test translated program '%s'".format(getFileName(fileName))) {
      val txt = loadFromCorrectFile(fileName)
      val p = new Parser(inputenc.StringTranslator.translate(txt), 3)
      val cs = p.checkSyntax()
      assert(cs.isEmpty)
    }
}
