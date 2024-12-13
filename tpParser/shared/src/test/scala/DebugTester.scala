import org.scalatest._

import tigerpython.utilities.completer._

/**
 * This 'test' is intended to run individual cases so as to debug the parser.  It does nothing in the general case.
 */
class DebugTester extends FunSuite  {

/*  val PROGRAM: String =
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
}
