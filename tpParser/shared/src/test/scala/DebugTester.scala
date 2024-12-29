import org.scalatest._
import tigerpython.parser.Parser
import tigerpython.utilities.completer._

/**
 * This 'test' is intended to run individual cases so as to debug the parser.  It does nothing in the general case.
 */
class DebugTester extends FunSuite  {

  private def loadFromFile(fileName: String): String = {
    val src = scala.io.Source.fromFile(fileName)
    src.getLines().mkString("\n")
  }

  /*  private val PROGRAM: String =
      """import math
        |['a'][0].""".stripMargin

    {
      val completer = new Completer("<module>", PROGRAM, PROGRAM.length)
      val nameFilter = completer.getNameFilter
      if (nameFilter != null) {
        val suggestions = nameFilter.getNameList("")
        println(suggestions.mkString(";"))
        println("– done –")
      } else
        println("– no entries –")
    }*/

  {
    /*val program = loadFromFile("./tpParser/shared/src/test/programs/completer/typeshed/turtle.pyi")
    val t1 = System.currentTimeMillis()
    val p = new Parser(program, 3)
    val cs = p.checkSyntax()
    if (cs.nonEmpty)
      println(cs.get.toString, cs.get.position)
    val t2 = System.currentTimeMillis()
    println(s"Parsing done in ${t2 - t1} ms")*/
  }
}
