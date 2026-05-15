package tigerpython.parser.parsing

import tigerpython.parser.errors.ErrorCode

/**
 * Some kinds of errors show up early on in the parsing process as *possible* errors, but need a full AST or even
 * type analysis in order to be really confirmed as actual errors.  For instance, it is rather common for some
 * students to write `pi = 3,1415` with a comma instead of a period.  This is valid Python code, creating a tuple,
 * which makes it difficult to outright reject that code.  However, using type analysis, we might be able to confirm
 * that `pi` is used as a number, allowing us to reason backward and issue a corresponding error message.
 *
 * For such cases, the parser will record a `Suspicion`, which is to be resolved later on, once the fully annotated
 * AST has been established.
 *
 * @author Tobias Kohn
 */
class Suspicion(val parserState: ParserState,
                val pos: Int,
                val code: ErrorCode.Value,
                val params: Seq[AnyRef]) {

  def resolve(): Unit = {}

  /**
   * Marks the suspicion as resolved, removing it from the list of suspicions to check.
   */
  def resolved(): Unit = {
    val idx = parserState.suspicions.indexOf(this)
    if (idx >= 0)
      parserState.suspicions.remove(idx)
  }
}
