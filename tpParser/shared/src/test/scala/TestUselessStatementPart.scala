import org.scalatest._
import tigerpython.parser.Parser
import tigerpython.parser.errormessages._
import tigerpython.parser.errors.{ErrorCode, ErrorHandler, ExtErrorInfo}

/**
  * Tests for `USELESS_STATEMENT_PART`: an expression statement whose outermost operation (an attribute access, a
  * subscript or a comparison) has no effect, even though some inner part of it (typically a call) does.
  *
  * The motivating case is a missing pair of parentheses on a method call, e.g. `load_sound("...").play`.  The parser
  * already reported `s.play` as `USELESS_STATEMENT`, but said nothing about `s().play`, because the call `s()` has a
  * side effect.
  *
  * Statements that were already reported (as `USELESS_STATEMENT`, `CALL_NEEDS_PARENTHESES`,
  * `SINGLE_EQUAL_SIGN_EXPECTED`, ...) must keep their existing error.  `USELESS_STATEMENT_PART` is only used for
  * statements that previously went unreported, so that clients can switch it off independently.
  */
class TestUselessStatementPart extends FunSuite {

  ErrorHandler.WANT_STACK_TRACE = true

  private val PART = "USELESS_STATEMENT_PART"

  // Uses the same parser settings as `TestErroneousPrograms`.
  private def check(source: String, rejectDeadCode: Boolean = true, strictCode: Boolean = true): Option[ExtErrorInfo] = {
    val p = new Parser(source, 3)
    p.rejectDeadCode = rejectDeadCode
    p.repeatStatement = true
    p.strictCode = strictCode
    p.checkSyntax()
  }

  // Describes the first error as a string, so that a failing assertion shows what was actually reported.
  private def describe(err: Option[ExtErrorInfo]): String =
    err match {
      case Some(e) => "%s on line %d".format(e.errorCode, e.line + 1)
      case None => "no error"
    }

  /** Asserts that the first error reported is `code`, on the (1-based) line `line`. */
  private def assertError(source: String, code: String, line: Int = 1, strictCode: Boolean = true): Unit =
    assert(describe(check(source, strictCode = strictCode)) == "%s on line %d".format(code, line),
      "for source:\n" + source)

  private def assertNoError(source: String, rejectDeadCode: Boolean = true): Unit =
    assert(describe(check(source, rejectDeadCode = rejectDeadCode)) == "no error", "for source:\n" + source)

  // ---------------------------------------------------------------------------------------------------------------
  // The new error

  test("attribute access on a call result (missing parentheses on a method call)") {
    assertError("s().play", PART)
  }

  test("motivating case: sound literal followed by '.play' without parentheses") {
    assertError(
      """def load_sound(data):
        |    return Sound(data)
        |
        |load_sound("UklGRiQAAABXQVZF").play
        |""".stripMargin, PART, line = 4)
  }

  test("subscript containing a call") {
    assertError("arr[f(1)]", PART)
  }

  test("attribute access on a subscript containing a call") {
    assertError("arr[f(2)].play", PART)
  }

  test("comparison with a call on the left") {
    assertError("f(3) < 5", PART)
  }

  test("comparison with a call on the right") {
    assertError("3 == f(4)", PART)
  }

  test("chained comparison containing a call") {
    assertError("0 < f(5) < 10", PART)
  }

  test("chained attribute accesses on a call result") {
    assertError("f().a.b", PART)
  }

  test("subscript without any call (previously unreported)") {
    assertError("instruments['piano']", PART)
  }

  test("reported on the correct line inside a function and a loop") {
    assertError(
      """def play_all(sounds):
        |    for s in sounds:
        |        s.get().play
        |""".stripMargin, PART, line = 3)
  }

  test("second statement on a line, after a semicolon") {
    assertError("f(); g().play", PART)
  }

  test("still reported in an 'except' block: only the 'try' body is exempt") {
    assertError(
      """try:
        |    f()
        |except ValueError:
        |    g().play
        |""".stripMargin, PART, line = 4)
  }

  test("message is defined in every language and (for now) matches USELESS_STATEMENT") {
    val code = ErrorCode.withName(PART)
    val languages = Seq(EnglishMessages, GermanMessages, FrenchMessages, ItalianMessages, DutchMessages,
      RussianMessages)
    for (lang <- languages) {
      val msg = lang.getMessage(code)
      // EnglishMessages falls back to the code's name, and the others to `null`, if a message is missing.
      assert(msg != null && msg.nonEmpty && msg != PART, "missing message in " + lang)
      assert(msg == lang.getMessage(ErrorCode.USELESS_STATEMENT), "message mismatch in " + lang)
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Existing errors that must not change

  test("attribute access on a plain name stays USELESS_STATEMENT") {
    assertError("s.play", "USELESS_STATEMENT")
  }

  test("comparison without any call stays USELESS_STATEMENT") {
    assertError("x < 5", "USELESS_STATEMENT")
  }

  test("bare function name stays CALL_NEEDS_PARENTHESES") {
    assertError(
      """def f():
        |    pass
        |
        |f
        |""".stripMargin, "CALL_NEEDS_PARENTHESES", line = 4)
  }

  test("'x == 3' stays SINGLE_EQUAL_SIGN_EXPECTED") {
    assertError("x == 3", "SINGLE_EQUAL_SIGN_EXPECTED")
  }

  test("'x == f(3)' stays SINGLE_EQUAL_SIGN_EXPECTED") {
    assertError("x == f(3)", "SINGLE_EQUAL_SIGN_EXPECTED")
  }

  test("'x == 3' stays USELESS_STATEMENT without strict code") {
    assertError("x == 3", "USELESS_STATEMENT", strictCode = false)
  }

  test("arithmetic on a call stays USELESS_COMPUTATION") {
    assertError("f(1) + 2", "USELESS_COMPUTATION")
  }

  test("arithmetic on a name stays USELESS_STMT_USE_AUG_ASSIGN") {
    assertError("x + 1", "USELESS_STMT_USE_AUG_ASSIGN")
  }

  // (With a call to `f` instead, the parser reports `f` as a misspelled `if`.)
  test("comparison followed by a colon and a body stays MISSING_TOKEN (missing 'if')") {
    assertError(
      """get_value(3) < 5:
        |    print(1)
        |""".stripMargin, "MISSING_TOKEN")
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Must not be reported

  test("method calls with parentheses") {
    assertNoError(
      """def load_sound(data):
        |    return Sound(data)
        |
        |s.play()
        |s().play()
        |arr[f(1)].play()
        |load_sound("UklGRiQAAABXQVZF").play()
        |""".stripMargin)
  }

  test("attribute accesses, subscripts and comparisons that are used") {
    assertNoError(
      """x = s().play
        |arr[f(1)] = 3
        |s().volume = 5
        |del arr[f(1)]
        |print(f(3) < 5)
        |""".stripMargin)
  }

  test("checking whether a name exists in a 'try' body") {
    assertNoError(
      """try:
        |    name
        |except NameError:
        |    from module import name
        |""".stripMargin)
  }

  test("subscript and attribute access in a 'try' body") {
    assertNoError(
      """try:
        |    instruments['piano']
        |    load_sound("UklGRiQAAABXQVZF").play
        |except (KeyError, AttributeError):
        |    pass
        |""".stripMargin)
  }

  test("not reported when dead code is not rejected") {
    assertNoError("s().play", rejectDeadCode = false)
  }

  test("not reported in eval mode") {
    val p = new Parser("s().play", 3)
    p.rejectDeadCode = true
    p.strictCode = true
    p.parser.parserState.evalMode = true
    assert(describe(p.checkSyntax()) == "no error")
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Must not hide a genuine syntax error.  Errors are sorted by position, and USELESS_STATEMENT_PART is reported at
  // the start of the statement, so it would take precedence over a syntax error later in the same statement.

  for (source <- Seq("s().", "arr[f(1)", "arr[f(2)].", "s()[1", "arr[f(1)\nx = 1", "f(3) <", "3 == f(4",
                     "f(3) < 5 +"))
    test("syntax error in '%s' is reported as it would be without dead code checks".format(source.replace("\n", "\\n"))) {
      val withCheck = check(source)
      val withoutCheck = check(source, rejectDeadCode = false)
      assert(withoutCheck.isDefined, "expected a syntax error")
      assert(describe(withCheck) == describe(withoutCheck))
      assert(withCheck.get.position == withoutCheck.get.position)
    }
}
