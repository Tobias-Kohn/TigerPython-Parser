package tigerpython.parser

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

import errors.ExtErrorInfo
import tigerpython.inputenc.StringTranslator

/**
  * The `TPyParser` is the interface for using the parser in a JavaScript setting.  It provides the means to check a
  * given Python program for possible syntax errors.  All errors are reported as `ErrorInfo` instances with fields for
  * the error's position and the associated error message.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15/06/2016
  * Updated by Tobias Kohn on 30/03/2020
  */
@JSExportTopLevel("TPyParser")
object TPyParser {

  /**
    * Consider expressions to be complete statements.
    */
  @JSExport
  var evalMode: Boolean = false

  /**
    * Use the division from Python 3 even in Python 2.
    */
  @JSExport
  var newDivision: Boolean = true

  /**
    * Set the Python version to either 2 or 3.
    */
  @JSExport
  var pythonVersion: Int = 3

  /**
    * Reject dead code after 'return' etc.
    */
  @JSExport
  var rejectDeadCode: Boolean = true

  /**
    * Include the 'repeat'-statement from TigerJython.
    */
  @JSExport
  var repeatStatement: Boolean = false

  /**
    * Use sage's convention of `^` as power-operator.
    */
  @JSExport
  var sagePower: Boolean = false

  /**
    * Consider unlikely patterns as errors, i. e. missing side-effects.
    */
  @JSExport
  var strictCode: Boolean = false

  /**
    * Translate non-ASCII punctuation marks (such as in CJK texts).
    */
  @JSExport
  var translateUnicodePunctuation: Boolean = false

  /**
    * Treat all warnings as errors.
    */
  @JSExport
  var warningAsErrors: Boolean = true

  /**
    * Returns all the available languages.
    */
  @JSExport
  def getLanguages(): js.Array[String] = errors.ErrorTranslator.errorTranslator.languages.toJSArray

  /**
    * Returns the currently set language in which error messages are printed.
    */
  @JSExport
  def getLanguage(): String = errors.ErrorTranslator.errorTranslator.language

  /**
    * Sets the language in which error messages are printed.
    */
  @JSExport
  def setLanguage(language: String): Unit = {
    errors.ErrorTranslator.errorTranslator.language = language
  }

  @JSExport
  def setErrorMessage(code: String, msg: String): Unit =
    errors.ErrorTranslator.errorTranslator.setMessage(errors.ErrorCode.withName(code), msg)

  /**
    * Checks the syntax of the given source code and returns either `null` or the first error detected in the code.
    *
    * @param source  The entire Python program as a single string.
    * @return        Either `null` or an instance of the `ErrorInfo` structure.
    */
  @JSExport
  def checkSyntax(source: String): ErrorInfo = {
    val src =
      if (translateUnicodePunctuation)
        StringTranslator.translate(source)
      else
        source
    val parser = new Parser(src, pythonVersion)
    parser.newDivision = newDivision
    parser.rejectDeadCode = rejectDeadCode
    parser.repeatStatement = repeatStatement
    parser.sagePower = sagePower
    parser.strictCode = strictCode
    parser.checkSyntax() match {
      case Some(ExtErrorInfo(pos, _, code, msg, _)) =>
        val line = parser.lineFromPosition(pos)
        val offset = parser.lineOffsetFromPosition(pos)
        ErrorInfo(line, offset, msg, code.toString)
      case None =>
        null
    }
  }

  /**
    * Checks the syntax of the given source code and returns a list with all errors detected in the code.
    *
    * @param source  The entire Python program as a single string.
    * @return        A (possibly empty) JavaScript-array where each item is an instance of the `ErrorInfo` structure.
    */
  @JSExport
  def findAllErrors(source: String): js.Array[ErrorInfo] = {
    val src =
      if (translateUnicodePunctuation)
        StringTranslator.translate(source)
      else
        source
    val parser = new Parser(src, pythonVersion)
    parser.newDivision = newDivision
    parser.rejectDeadCode = rejectDeadCode
    parser.repeatStatement = repeatStatement
    parser.sagePower = sagePower
    parser.strictCode = strictCode
    parser.checkSyntaxAll().map(item => {
      val line = parser.lineFromPosition(item.position)
      val offset = parser.lineOffsetFromPosition(item.position)
      ErrorInfo(line, offset, item.errorMessage, item.errorCode.toString)
    }).toJSArray
  }

  /**
    * Parses the given source code and returns the AST.
    *
    * @param source  The entire Python program as a single string.
    * @return
    */
  @JSExport
  def parse(source: String): js.Any = {
    val src =
      if (translateUnicodePunctuation)
        StringTranslator.translate(source)
      else
        source
    val parser = new Parser(src, pythonVersion)
    parser.newDivision = newDivision
    parser.rejectDeadCode = rejectDeadCode
    parser.repeatStatement = repeatStatement
    parser.sagePower = sagePower
    parser.strictCode = strictCode
    val converter = new AstConverter(parser)
    val ast = parser.parse()
    converter(ast)
  }
}
