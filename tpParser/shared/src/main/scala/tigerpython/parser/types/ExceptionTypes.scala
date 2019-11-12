/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 29/06/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
object ExceptionTypes {
  private[parser] val errors = collection.mutable.Map[String, PrimitiveType]()
  
  private def ErrorType(name: String, base: PrimitiveType = null): PrimitiveType = {
    val result = if (base != null)
      PrimitiveType(name, base)
    else
      PrimitiveType(name)
    errors(name) = result
    result
  }

  def byName(name: String): PrimitiveType = errors.getOrElse(name, null)

  final val BASE_EXCEPTION = ErrorType("BaseException")
  final val SYSTEM_EXIT = ErrorType("SystemExit", BASE_EXCEPTION)
  final val KEYBOARD_INTERRUPT = ErrorType("KeyboardInterrupt", BASE_EXCEPTION)
  final val GENERATOR_EXIT = ErrorType("GeneratorExit", BASE_EXCEPTION)
  final val EXCEPTION = ErrorType("Exception", BASE_EXCEPTION)
  final val STOP_ITERATION = ErrorType("StopIteration", EXCEPTION)

  final val STANDARD_ERROR = ErrorType("StandardError", EXCEPTION)
  final val BUFFER_ERROR = ErrorType("BufferError", STANDARD_ERROR)
  final val ARITHMETIC_ERROR = ErrorType("ArithmeticError", STANDARD_ERROR)
  final val FLOATING_POINT_ERROR = ErrorType("FloatingPointError", ARITHMETIC_ERROR)
  final val OVERFLOW_ERROR = ErrorType("OverflowError", ARITHMETIC_ERROR)
  final val ZERO_DIVISION_ERROR = ErrorType("ZeroDivisionError", ARITHMETIC_ERROR)
  final val ASSERTION_ERROR = ErrorType("AssertionError", STANDARD_ERROR)
  final val ATTRIBUTE_ERROR = ErrorType("AttributeError", STANDARD_ERROR)
  final val ENVIRONMENT_ERROR = ErrorType("EnvironmentError", STANDARD_ERROR)
  final val IO_ERROR = ErrorType("IOError", ENVIRONMENT_ERROR)
  final val OS_ERROR = ErrorType("OSError", ENVIRONMENT_ERROR)
  final val WINDOWS_ERROR = ErrorType("WindowsError", OS_ERROR)
  final val VMS_ERROR = ErrorType("VMSError", OS_ERROR)
  final val EOF_ERROR = ErrorType("EOFError", STANDARD_ERROR)
  final val IMPORT_ERROR = ErrorType("ImportError", STANDARD_ERROR)
  final val LOOKUP_ERROR = ErrorType("LookupError", STANDARD_ERROR)
  final val INDEX_ERROR = ErrorType("IndexError", LOOKUP_ERROR)
  final val KEY_ERROR = ErrorType("KeyError", LOOKUP_ERROR)
  final val MEMORY_ERROR = ErrorType("MemoryError", STANDARD_ERROR)
  final val NAME_ERROR = ErrorType("NameError", STANDARD_ERROR)
  final val UNBOUND_LOCAL_ERROR = ErrorType("UnboundLocalError", NAME_ERROR)
  final val REFERENCE_ERROR = ErrorType("ReferenceError", STANDARD_ERROR)
  final val RUNTIME_ERROR = ErrorType("RuntimeError", STANDARD_ERROR)
  final val NOT_IMPLEMENTED_ERROR = ErrorType("NotImplementedError", RUNTIME_ERROR)
  final val SYNTAX_ERROR = ErrorType("SyntaxError", STANDARD_ERROR)
  final val INDENTATION_ERROR = ErrorType("IndentationError", SYNTAX_ERROR)
  final val TAB_ERROR = ErrorType("TabError", INDENTATION_ERROR)
  final val SYSTEM_ERROR = ErrorType("SystemError", STANDARD_ERROR)
  final val TYPE_ERROR = ErrorType("TypeError", STANDARD_ERROR)
  final val VALUE_ERROR = ErrorType("ValueError", STANDARD_ERROR)
  final val UNICODE_ERROR = ErrorType("UnicodeError", VALUE_ERROR)
  final val UNICODE_DECODE_ERROR = ErrorType("UnicodeDecodeError", UNICODE_ERROR)
  final val UNICODE_ENCODE_ERROR = ErrorType("UnicodeEncodeError", UNICODE_ERROR)
  final val UNICODE_TRANSLATE_ERROR = ErrorType("UnicodeTranslateError", UNICODE_ERROR)

  final val WARNING = ErrorType("Warning", EXCEPTION)
  final val DEPRECATION_WARNING = ErrorType("DeprecationWarning", EXCEPTION)
  final val PENDING_DEPRECATION_WARNING = ErrorType("PendingDeprecationWarning", EXCEPTION)
  final val RUNTIME_WARNING = ErrorType("RuntimeWarning", EXCEPTION)
  final val SYNTAX_WARNING = ErrorType("SyntaxWarning", EXCEPTION)
  final val USER_WARNING = ErrorType("UserWarning", EXCEPTION)
  final val FUTURE_WARNING = ErrorType("FutureWarning", EXCEPTION)
  final val IMPORT_WARNING = ErrorType("ImportWarning", EXCEPTION)
  final val UNICODE_WARNING = ErrorType("UnicodeWarning", EXCEPTION)
  final val BYTES_WARNING = ErrorType("BytesWarning", EXCEPTION)

}
