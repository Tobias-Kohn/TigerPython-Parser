import org.scalatest._
import tigerpython.utilities.completer._
import tigerpython.utilities.scopes.ModuleLoader

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
    val leadingComments = lines.takeWhile(_.startsWith("#"))
    if (lines.length > 2) {
      if (leadingComments.length > 2) {
        // If there's more than two comments line, the earlier set are taken to be a pre-declared module.
        // The first line is the module name, then everything after that (before the final two lines of comments)
        // are the contents of the module.  We remove "#" at the start of the string but no more, because
        // some of the contents might be meaningfully indented so we can't just trim every line:
        val module_name = leadingComments(0).drop(1).trim
        val module_contents = leadingComments.slice(1, leadingComments.length - 2).map((x) => if (x.length < 2) x else x.substring(2))
        ModuleLoader.addModule(module_name, module_contents)
        // Note: the module contents are not cleared between tests, so it's a good idea to name
        // all involved modules uniquely to avoid interference.
      }

      val pos = leadingComments(leadingComments.length - 2).dropWhile(!_.isDigit).takeWhile(_.isDigit).toInt
      val expected_result = lines(leadingComments.length - 1).drop(1).trim
      val text = lines.dropWhile(_.startsWith("#")).mkString("\n")
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

  for (fileName <- listAllFiles("completer_params"))
    test("test program '%s'".format(getFileName(fileName))) {
      val (pos, expected_result, text) = loadFromCompleterFile(fileName)
      val completer = new Completer(fileName, text, pos)
      val funcNameAtPos = text.drop(pos).takeWhile(_ != '(')
      val params = completer.getNameFilter.getParams(funcNameAtPos)
      assert(params == expected_result)
      val extParams = completer.getNameFilter.getExtInfoList.filter(_.name == funcNameAtPos).map(_.parameters.mkString(", "))
      assert(extParams sameElements Array(expected_result))
    }
}
