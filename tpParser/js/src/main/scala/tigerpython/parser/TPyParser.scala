package tigerpython.parser

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import errors.ExtErrorInfo
import tigerpython.inputenc.StringTranslator
import tigerpython.utilities.completer.Completer
import tigerpython.utilities.scopes.ModuleLoader
import tigerpython.utilities.types.{SignatureArg, SignatureVarArg}

import scala.annotation.tailrec

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
  def getLanguages: js.Array[String] = errors.ErrorTranslator.errorTranslator.languages.toJSArray

  /**
    * Returns the currently set language in which error messages are printed.
    */
  @JSExport
  def getLanguage: String = errors.ErrorTranslator.errorTranslator.language

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
   * TigerPython comes with some small modifications to the Python defaults, which are enabled by this method.  Note
   * that this cannot be undone; the modifications to the types and built-in dictionary are persistent.
   */
  @JSExport
  def enableTigerPythonModifications(): Unit = {
    tigerpython.utilities.types.BuiltinTypes.appyTigerPythonModifications()
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

  /**
   * Returns a list of auto-complete suggestions, based on the source file and the current position.  The position
   * is given as an integer offset with the absolute location from the beginning of the text.
   *
   * If you set `filter` to true, the returned list will be filtered according to the identifier left to the position
   * (if any).
   */
  @JSExport
  def autoComplete(source: String, pos: Int, filter: Boolean = false): js.Array[String] = {
    val completer = new Completer("<module>", source, pos)
    val cur_name = completer.getCurrentName
    if (filter && cur_name != null) {
      val cur_name_len = cur_name.length
      var i = pos
      while (i >= 0 && i + cur_name_len >= pos && source.slice(i, i + cur_name_len) != cur_name)
        i -= 1
      if (i >= 0 && source.slice(i, i + cur_name_len) == cur_name)
        return completer.getNameFilter.getNameList(cur_name.take(pos - i)).toJSArray
    }
    completer.getNameFilter.getNameList("").toJSArray
  }

  @JSExport
  def autoCompleteExt(source: String, pos: Int): js.Any = {
    val completer = new Completer("<module>", source, pos)
    val nameFilter = completer.getNameFilter
    if (nameFilter == null) {
      return null
    }
    val makeSignatureArg = (a : SignatureArg) => js.Dynamic.literal(
      name = a.name,
      defaultValue = a.defaultValue.getOrElse(null),
      argType = a.argType.getTypeName
    )
    val makeSignatureVarArg = (a : SignatureVarArg) => js.Dynamic.literal(
      name = a.name,
      argType = a.argType.getTypeName
    )


    val items = nameFilter.getExtInfoList
    (for (item <- items)
      yield js.Dynamic.literal(
        acResult = item.name,
        documentation = item.documentation,
        `type` = item.itemType,
        params = if (item.parameters != null)
          item.parameters.toJSArray
        else
          null,
        signature = if (item.signature != null) {
          js.Dynamic.literal(
            positionalOnlyArgs = item.signature.positionalOnlyArgs.map(makeSignatureArg).toJSArray,
            positionalOrKeywordArgs = item.signature.positionalOrKeywordArgs.map(makeSignatureArg).toJSArray,
            varArgs = item.signature.varArgs.map(makeSignatureVarArg).getOrElse(null),
            keywordOnlyArgs = item.signature.keywordOnlyArgs.map(makeSignatureArg).toJSArray,
            varKwargs = item.signature.varKwargs.map(makeSignatureVarArg).getOrElse(null),
            firstParamIsSelf = item.signature.firstParamIsSelf
          )
        }
        else
          null
      )
    ).toJSArray
  }

  /**
   * Returns the (qualified) name at the current position.  If the name stems from a specific module such as `turtle`,
   * say, it will be returned as `turtle.name`, allowing for a (quasi) unique identification of the name under the
   * cursor.
   */
  @JSExport
  def getQualifiedName(source: String, pos: Int): String = {
    val completer = new Completer("<module>", source, pos)
    completer.getCurrentName
  }

  /**
   * Adds a new Python module that can then be used for auto-completion.
   */
  @JSExport
  @tailrec
  def defineModule(moduleName: String, moduleBody: String, sourceFormat: String = null): Unit =
    if (sourceFormat != null)
      sourceFormat.toLowerCase match {
        case "pyi" =>
          ModuleLoader.addPyiModule(moduleName, moduleBody)
        case "legacy" | "tj" =>
          ModuleLoader.addModule(moduleName, moduleBody.split('\n'))
        case _ =>
          defineModule(moduleName, moduleBody, "legacy")
      }
    else
      defineModule(moduleName, moduleBody, "legacy")
}
