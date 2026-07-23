import org.scalatest._
import tigerpython.parser.Parser
import tigerpython.parser.errors.ErrorHandler
import tigerpython.parser.printer.{AstEquivalence, AstPrinter}

/**
  * Round-trip regression test: parse -> print -> reparse -> compare.
  *
  * For every existing fixture (both `programs/correct` and `programs/erroneous`), this
  * checks that the AST produced by the initial parse (which may have gone through
  * error-recovery for the `erroneous` fixtures) can be printed back to Python source
  * that (a) reparses without any errors, and (b) reparses to a structurally equivalent
  * AST (ignoring source positions). This is a regression net for the printer
  * and, more importantly, for the parser's error-recovery logic: a genuinely "fixed"
  * program should behave like the well-formed program it was corrected to.
  *
  * Deliberately not checking exact-text round-trip (`unparse(parse(src)) == src`):
  * that's meaningless for the `erroneous` fixtures (the corrected text can never equal
  * the malformed original) and would require a format-preserving printer this project
  * has no other use for.
  */
class TestRoundTrip extends FunSuite {

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

  // Fixtures excluded for now: the printer targets Python 3 output at the moment
  // so the one Python-2 fixture is not yet expected to round-trip.
  private val skip: Set[String] = Set("vigenere")

  private def loadCorrectSource(fileName: String): String =
    scala.io.Source.fromFile(fileName).getLines().mkString("\n")

  private def loadErroneousSource(fileName: String): String = {
    val lines = scala.io.Source.fromFile(fileName).getLines().toArray
    if (lines.length > 2) lines.drop(2).mkString("\n") else ""
  }

  ErrorHandler.WANT_STACK_TRACE = true

  private def roundTrip(name: String, source: String, pythonVersion: Int): Unit = {
    // Deliberately not enabling `repeatStatement` (TigerJython's non-standard `repeat`
    // loop, which also reserves `repeat` as a keyword): most fixtures don't use it and
    // never intended `repeat` to be reserved, and reserving it turns any unrelated use
    // of `repeat` as an ordinary identifier (e.g. a method named `repeat`) into a
    // degenerate, unprintable recovery artifact (a `Name` node with a `null` name).
    //
    // Also deliberately not enabling `rejectDeadCode`/`strictCode`: these flag
    // additional, syntactically-valid-but-suspect *style* patterns (e.g. a bare name
    // as a whole statement), not syntax errors. Recovery can legitimately turn one
    // problem into simpler code that trips a *different*, unrelated style opinion
    // (e.g. `x++` recovered down to bare `x`, which is then itself flagged as a
    // "useless statement") - that's a style concern about the recovered code, not a
    // sign that recovery produced something structurally wrong, so it shouldn't fail
    // this round-trip property.
    val p1 = new Parser(source, pythonVersion)
    val ast1 = p1.parse()
    if (ast1 == null)
      cancel(s"'$name': file cannot be parsed initially, so no point attempting a round-trip parse test." +
        s"\n\noriginal source:\n$source")
    // Errors already present after the *first* parse (e.g. a permanently-flagged but
    // still-recoverable dialect mismatch, such as a Python-2 `print` statement parsed
    // under Python 3) are allowed to recur after reprinting; only *new* kinds of errors
    // introduced by reprinting are treated as a round-trip failure.
    val errorCodes1 = p1.errorHandler.getAllErrors.map(_.errorCode).toSet

    val printed =
      try
        AstPrinter.unparse(ast1)
      catch {
        case e: Exception =>
          fail(s"printer failed on '$name': ${e.getMessage}\n\noriginal source:\n$source")
      }

    val p2 = new Parser(printed, pythonVersion)
    // NOTE: `parse()` must be called exactly once per Parser instance. The inner
    // `parsing.Parser.parse()` is not idempotent (re-running it drains an already
    // consumed token stream), so error-checking must read from `errorHandler`
    // after the single `parse()` call rather than via a second `checkSyntax*()`
    // call, which would itself re-invoke `parser.parse()`.
    val ast2 = p2.parse()
    val errors2 = p2.errorHandler.getAllErrors
    val newErrorCodes = errors2.map(_.errorCode).toSet -- errorCodes1
    if (newErrorCodes.nonEmpty)
      fail(s"reprinted source for '$name' introduced new error(s) not present after the first parse " +
        s"(${newErrorCodes.mkString(", ")}): ${errors2.mkString("; ")}" +
        s"\n\nreprinted source:\n$printed\n\noriginal source:\n$source")

    AstEquivalence.equivalent(ast1, ast2) match {
      case Left(msg) =>
        fail(s"round-trip AST mismatch for '$name': $msg\n\nreprinted source:\n$printed\n\noriginal source:\n$source")
      case Right(()) =>
    }
  }

  for (fileName <- listAllFiles("correct") if !skip.contains(getFileName(fileName)))
    test("round-trip correct program '%s'".format(getFileName(fileName))) {
      roundTrip(getFileName(fileName), loadCorrectSource(fileName), getPythonVersion(fileName))
    }

  for (fileName <- listAllFiles("erroneous") if !skip.contains(getFileName(fileName)))
    test("round-trip erroneous program '%s'".format(getFileName(fileName))) {
      roundTrip(getFileName(fileName), loadErroneousSource(fileName), getPythonVersion(fileName))
    }
}
