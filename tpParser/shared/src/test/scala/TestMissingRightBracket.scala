import org.scalatest._
import tigerpython.parser.Parser
import tigerpython.parser.errors.ErrorHandler

/**
  * Tests for a name followed by an unclosed bracket where the bracketed expression runs to the very end of the
  * program, e.g. `arr[1`.  The bracket patcher used to crash with a `NullPointerException` on these, instead of
  * reporting the missing bracket.
  */
class TestMissingRightBracket extends FunSuite {

  ErrorHandler.WANT_STACK_TRACE = true

  // Describes the first error as a string, so that a failing assertion shows what was actually reported.
  private def firstError(source: String): String = {
    val p = new Parser(source, 3)
    p.checkSyntax() match {
      case Some(e) => "%s on line %d".format(e.errorCode, e.line + 1)
      case None => "no error"
    }
  }

  private val cases = Seq(
    ("arr[1", 1),
    ("arr[1\n", 1),
    ("arr[f(1)", 1),
    ("x = arr[f(1)", 1),
    ("arr[1, 2", 1),
    ("arr[1:2", 1),
    ("arr[1:2, 3", 1),
    ("print(arr[1", 1),
    ("x = 1\nprint(x)\ny = arr[x", 3),
    ("def f(x):\n    return x[0", 2)
  )

  for ((source, line) <- cases)
    test("unclosed bracket at end of input: '%s'".format(source.replace("\n", "\\n"))) {
      assert(firstError(source) == "MISSING_RIGHT_BRACKET on line %d".format(line))
    }
}
