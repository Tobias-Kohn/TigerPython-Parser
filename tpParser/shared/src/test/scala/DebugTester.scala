import org.scalatest._
import tigerpython.parser.Parser
import tigerpython.utilities.completer._
import tigerpython.utilities.scopes.ModuleLoader

/**
 * This 'test' is intended to run individual cases so as to debug the parser.  It does nothing in the general case.
 */
class DebugTester extends FunSuite  {

  private def loadFromFile(fileName: String): String = {
    val src = scala.io.Source.fromFile(fileName)
    src.getLines().mkString("\n")
  }

  private val PROGRAM: String =
    """def foo(x=+y=+z):
      |    print(x)
      |
      |x = 123""".stripMargin

  {
/*    val pyiSource = loadFromFile("./tpParser/shared/src/test/programs/completer/typeshed/turtle.pyi")
    ModuleLoader.addPyiModule("turtle", pyiSource)
    val completer = new Completer("<module>", PROGRAM, PROGRAM.length)
    val nameFilter = completer.getNameFilter*/
    /*if (nameFilter != null) {
      val suggestions = nameFilter.getNameList("")
      println(suggestions.mkString(";"))
      println("– done –")
    } else
      println("– no entries –")*/
    /*val p = new Parser(PROGRAM)
    p.repeatStatement = true
    val cs = p.checkSyntax()
    if (cs.nonEmpty) {
      val pos = cs.get.position
      println("ERROR")
      println(PROGRAM.slice(0, pos))
      println("???")
      println(PROGRAM.slice(pos, PROGRAM.length))
    }
    assert(cs.isEmpty)
    print("DONE")*/
  }
}
