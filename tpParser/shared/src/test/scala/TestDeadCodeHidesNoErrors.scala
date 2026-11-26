import org.scalatest._
import tigerpython.parser.Parser
import tigerpython.parser.errors.{ErrorHandler, ExtErrorInfo}

/**
  * The checks for useless statements (`USELESS_STATEMENT`, `USELESS_COMPUTATION`, ...) report their error at the
  * start of the statement.  Since errors are sorted by position, such an error would take precedence over (and so
  * hide) a genuine syntax error further along the same statement, e.g. `NAME_EXPECTED` in `s.`.  These tests check
  * that the syntax error is reported exactly as it would be without dead code checks.
  */
class TestDeadCodeHidesNoErrors extends FunSuite {

  ErrorHandler.WANT_STACK_TRACE = true

  // Uses the same parser settings as `TestErroneousPrograms`.
  private def check(source: String, rejectDeadCode: Boolean = true): Option[ExtErrorInfo] = {
    val p = new Parser(source, 3)
    p.rejectDeadCode = rejectDeadCode
    p.repeatStatement = true
    p.strictCode = true
    p.checkSyntax()
  }

  private def describe(err: Option[ExtErrorInfo]): String =
    err match {
      case Some(e) => "%s at position %d".format(e.errorCode, e.position)
      case None => "no error"
    }

  private def escape(source: String): String = source.replace("\n", "\\n")

  // ---------------------------------------------------------------------------------------------------------------
  // Syntax errors that must not be hidden

  private val incomplete = Seq(
    // Formerly hidden by USELESS_STATEMENT
    "s.", "s.play.", "a.b.", "x .", "s. ; y = 1", "s.\ny = 1",
    // Formerly hidden by CALL_NEEDS_PARENTHESES
    "def f():\n    pass\n\nf.",
    // Formerly hidden by USELESS_COMPUTATION
    "x +", "x + 3 *", "3 +", "-x +", "f() +", "f() + 3 *", "x.y +", "x[1] +", "x and", "x or", "(x +)", "x +\ny = 1",
    // Not affected, but included for completeness
    "x <", "x < 3 <", "s().", "x in", "not"
  )

  for (source <- incomplete)
    test("syntax error in '%s' is not hidden by a dead code error".format(escape(source))) {
      val withoutCheck = check(source, rejectDeadCode = false)
      assert(withoutCheck.isDefined, "expected a syntax error")
      assert(describe(check(source)) == describe(withoutCheck))
    }

  // ---------------------------------------------------------------------------------------------------------------
  // Complete statements are still reported

  // (source, expected error, line number of the error)
  private val complete = Seq(
    ("s.play", "USELESS_STATEMENT", 1),
    ("a.b.c", "USELESS_STATEMENT", 1),
    ("x < 3", "USELESS_STATEMENT", 1),
    ("-x", "USELESS_STATEMENT", 1),
    ("x + 1", "USELESS_STMT_USE_AUG_ASSIGN", 1),
    ("f() + 1", "USELESS_COMPUTATION", 1),
    ("s().play", "USELESS_STATEMENT_PART", 1),
    ("s", "USELESS_STATEMENT", 1),
    ("def f():\n    pass\n\nf", "CALL_NEEDS_PARENTHESES", 4),
    // An error in a different statement does not prevent the report
    ("s.play\ny.", "USELESS_STATEMENT", 1),
    ("s\ny.", "USELESS_STATEMENT", 1),
    ("s.play; y.", "USELESS_STATEMENT", 1),
    ("x + 1\ny +", "USELESS_STMT_USE_AUG_ASSIGN", 1)
  )

  for ((source, code, line) <- complete)
    test("'%s' is still reported as %s".format(escape(source), code)) {
      val err = check(source)
      assert(err.map(e => "%s on line %d".format(e.errorCode, e.line + 1)) == Some("%s on line %d".format(code, line)))
    }
}
