/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.errors

/**
  * The abstract list of all error messages.
  * These need to be translated to a human readable format by a `ErrorTranslator`.
  *
  * Scala would allow us to just use `Value` instead of the much more verbose `Value("...")`.  The reason for using the
  * string values is to make sure that these names are not optimised away or mangled, but remain fully available in the
  * compiled module.  Other modules might want to access these values instead of parsing the human readable error
  * message.
  *
  * All changes must be accounted for in the respective objects in `errormessages` to make sure that there is an actual
  * message to be displayed.  Due to the various languages involved, this might mean that additional translators have
  * to get involved.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 21/05/2016
  * Updated by Tobias Kohn on 18/01/2020
  */
object ErrorCode extends Enumeration {

  final val UNKNOWN = Value("UNKNOWN")

  final val AND_CONNECTS_CMP_NOT_VARS = Value("AND_CONNECTS_CMP_NOT_VARS")
  final val ARG_AFTER_VARARGS = Value("ARG_AFTER_VARARGS")
  final val AS_NOT_ALLOWED_HERE = Value("AS_NOT_ALLOWED_HERE")
  final val ASSIGNMENT_TO_RIGHT = Value("ASSIGNMENT_TO_RIGHT")
  final val BREAK_OUTSIDE_LOOP = Value("BREAK_OUTSIDE_LOOP")
  final val CALL_NEEDS_PARENTHESES = Value("CALL_NEEDS_PARENTHESES")
  final val CANNOT_APPLY_ASYNC = Value("CANNOT_APPLY_ASYNC")
  final val CANNOT_ASSIGN_TO_CALL = Value("CANNOT_ASSIGN_TO_CALL")
  final val CANNOT_ASSIGN_TO_FUNCTION = Value("CANNOT_ASSIGN_TO_FUNCTION")
  final val CANNOT_CALL_VALUE = Value("CANNOT_CALL_VALUE")
  final val CANNOT_REDEFINE_NAME = Value("CANNOT_REDEFINE_NAME")
  final val CANNOT_TEST_TUPLE = Value("CANNOT_TEST_TUPLE")
  final val CANNOT_USE_KEYWORD_AS_NAME = Value("CANNOT_USE_KEYWORD_AS_NAME")
  final val CLASS_METHOD_WITHOUT_SELF = Value("CLASS_METHOD_WITHOUT_SELF")
  final val COLON_EXPECTED = Value("COLON_EXPECTED")
  final val CONDITION_CANNOT_BE_FULFILLED = Value("CONDITION_CANNOT_BE_FULFILLED")
  final val CONDITION_ALWAYS_FULFILLED = Value("CONDITION_ALWAYS_FULFILLED")
  final val DECORATOR_NAME_CLASH = Value("DECORATOR_NAME_CLASH")
  final val DECORATOR_NEEDS_CALLABLE = Value("DECORATOR_NEEDS_CALLABLE")
  final val DEFINITION_INSIDE_LOOP = Value("DEFINITION_INSIDE_LOOP")
  final val DOUBLE_CALL = Value("DOUBLE_CALL")
  final val DOUBLE_EQUAL_SIGN_EXPECTED = Value("DOUBLE_EQUAL_SIGN_EXPECTED")
  final val DOUBLE_ELSE = Value("DOUBLE_ELSE")
  final val DOUBLE_PARAMETER_NAMES = Value("DOUBLE_PARAMETER_NAMES")
  final val ELSE_MUST_BE_INDENTED = Value("ELSE_MUST_BE_INDENTED")
  final val ELSE_WITH_COMPARISON = Value("ELSE_WITH_COMPARISON")
  final val ELSE_WITHOUT_IF = Value("ELSE_WITHOUT_IF")
  final val EMPTY_SUBSCRIPT = Value("EMPTY_SUBSCRIPT")
  final val EXTRA_BRACKETS = Value("EXTRA_BRACKETS")
  final val EXTRA_INDENTATION = Value("EXTRA_INDENTATION")
  final val EXTRA_LINEBREAK = Value("EXTRA_LINEBREAK")
  final val EXTRA_LEFT_BRACKET = Value("EXTRA_LEFT_BRACKET")
  final val EXTRA_LINE_NUMBER = Value("EXTRA_LINE_NUMBER")  //***
  final val EXTRA_RIGHT_BRACKET = Value("EXTRA_RIGHT_BRACKET")
  final val EXTRA_SPACE = Value("EXTRA_SPACE")
  final val EXTRA_SPACE_OR_MISSING_COMMA = Value("EXTRA_SPACE_OR_MISSING_COMMA")
  final val EXTRA_TOKEN = Value("EXTRA_TOKEN")
  final val FOREIGN_KEYWORD = Value("FOREIGN_KEYWORD")
  final val FOREIGN_PRIVATE = Value("FOREIGN_PRIVATE")
  final val FOREIGN_STATEMENT = Value("FOREIGN_STATEMENT")
  final val FOREIGN_SYNTAX = Value("FOREIGN_SYNTAX")
  final val FOREIGN_TOKEN = Value("FOREIGN_TOKEN")
  final val FOREIGN_VAR = Value("FOREIGN_VAR")
  final val FOR_TARGET_NAME_REQUIRED = Value("FOR_TARGET_NAME_REQUIRED")
  final val FUTURE_MUST_BE_FIRST = Value("FUTURE_MUST_BE_FIRST")
  final val GENERATOR_CANNOT_RETURN_VALUE = Value("GENERATOR_CANNOT_RETURN_VALUE")
  final val GLOBAL_MUST_BE_FIRST = Value("GLOBAL_MUST_BE_FIRST")
  final val GLOBAL_OUTSIDE_FUNCTION = Value("GLOBAL_OUTSIDE_FUNCTION")
  final val IMPORT_INSIDE_LOOP = Value("IMPORT_INSIDE_LOOP")
  final val INCOMPLETE_IMPORT = Value("INCOMPLETE_IMPORT")
  final val INCONSISTENT_INDENTATION = Value("INCONSISTENT_INDENTATION")
  final val INCONSISTENT_RETURNS = Value("INCONSISTENT_RETURNS")
  final val INDENTED_ELSE = Value("INDENTED_ELSE")
  final val INDENTED_HEADER = Value("INDENTED_HEADER")  //***
  final val INFINITE_LOOP = Value("INFINITE_LOOP")
  final val INITIALIZATION_INSIDE_LOOP = Value("INITIALIZATION_INSIDE_LOOP")
  final val INVALID_ASSIGNMENT = Value("INVALID_ASSIGNMENT")
  final val INVALID_AUGASSIGN_TARGET = Value("INVALID_AUGASSIGN_TARGET")
  final val INVALID_CONDITION = Value("INVALID_CONDITION")
  final val INVALID_FUNCTION_DEF = Value("INVALID_FUNCTION_DEF")
  final val INVALID_FUNCTION_DEF_ASSIGN = Value("INVALID_FUNCTION_DEF_ASSIGN")
  final val INVALID_GENERATOR_ARG = Value("INVALID_GENERATOR_ARG")
  final val INVALID_INPUT_CHARACTER = Value("INVALID_INPUT_CHARACTER")
  final val INVALID_KEY_VALUE_PAIR = Value("INVALID_KEY_VALUE_PAIR")
  final val INVALID_NAME = Value("INVALID_NAME")
  final val INVALID_STRING_PREFIX = Value("INVALID_STRING_PREFIX")
  final val INVALID_TOKEN_AT_START_OF_LINE = Value("INVALID_TOKEN_AT_START_OF_LINE")
  final val METHOD_WITHOUT_SELF = Value("METHOD_WITHOUT_SELF")
  final val MISMATCHED_CLOSING_BRACKET = Value("MISMATCHED_CLOSING_BRACKET")
  final val MISPLACED_ASSIGN = Value("MISPLACED_ASSIGN")
  final val MISSING_ASSIGNMENT = Value("MISSING_ASSIGNMENT")
  final val MISSING_ASSIGNMENT_SOURCE = Value("MISSING_ASSIGNMENT_SOURCE")
  final val MISSING_BODY = Value("MISSING_BODY")
  final val MISSING_COMMA = Value("MISSING_COMMA")
  final val MISSING_COMPARISON = Value("MISSING_COMPARISON")
  final val MISSING_DOT = Value("MISSING_DOT")
  final val MISSING_EXPRESSION = Value("MISSING_EXPRESSION")
  final val MISSING_LEFT_BRACKET = Value("MISSING_LEFT_BRACKET")
  final val MISSING_LEFT_PARENTHESIS = Value("MISSING_LEFT_PARENTHESIS")
  final val MISSING_OPERATOR_OR_COMMA = Value("MISSING_OPERATOR_OR_COMMA")
  final val MISSING_PARENTHESES = Value("MISSING_PARENTHESES")
  final val MISSING_RIGHT_BRACKET = Value("MISSING_RIGHT_BRACKET")
  final val MISSING_SPACE = Value("MISSING_SPACE")
  final val MISSING_TOKEN = Value("MISSING_TOKEN")
  final val MISSPELLED_KEYWORD = Value("MISSPELLED_KEYWORD")
  final val MISSPELLED_NUMBER = Value("MISSPELLED_NUMBER")
  final val MISSPELLED_OPERATOR = Value("MISSPELLED_OPERATOR")
  final val MULTIPLE_VAR_ARGS = Value("MULTIPLE_VAR_ARGS")
  final val MULTIPLE_VAR_PARAMS = Value("MULTIPLE_VAR_PARAMS")
  final val NAME_EXPECTED = Value("NAME_EXPECTED")
  final val NESTED_FUNCTIONS = Value("NESTED_FUNCTIONS")
  final val NO_END_NEEDED = Value("NO_END_NEEDED")
  final val NO_PARAM_DEFAULT_ALLOWED = Value("NO_PARAM_DEFAULT_ALLOWED")
  final val NO_VIABLE_ALTERNATIVE = Value("NO_VIABLE_ALTERNATIVE")
  final val NUMBER_NOT_SUBSCRIPTABLE = Value("NUMBER_NOT_SUBSCRIPTABLE")
  final val PARAM_AFTER_KEYWORD_PARAM = Value("PARAM_AFTER_KW")
  final val PARAMS_REQUIRED = Value("PARAMS_REQUIRED")
  final val POS_ARG_AFTER_KEYWORD = Value("POS_ARG_AFTER_KEYWORD")
  final val POS_PARAM_AFTER_KEYWORD = Value("POS_PARAM_AFTER_KEYWORD")
  final val PRINT_DEST_EXPECTED = Value("PRINT_DEST_EXPECTED")
  final val PRINT_IS_STATEMENT = Value("PRINT_IS_STATEMENT")
  final val PRINT_NEEDS_PARENTHESES = Value("PRINT_NEEDS_PARENTHESES")
  final val PYTHON_2_FEATURE_NOT_AVAILABLE = Value("PYTHON_2_FEATURE_NOT_AVAILABLE")
  final val PYTHON_3_FEATURE_NOT_AVAILABLE = Value("PYTHON_3_FEATURE_NOT_AVAILABLE")
  final val REPEAT_NOT_ENABLED = Value("REPEAT_NOT_ENABLED")
  final val RETURN_OUTSIDE_FUNCTION = Value("RETURN_OUTSIDE_FUNCTION")
  final val SINGLE_EQUAL_SIGN_EXPECTED = Value("SINGLE_EQUAL_SIGN_EXPECTED")
  final val SUPERFLUOUS_COMPARISON = Value("SUPERFLUOUS_COMPARISON")
  final val SWAPPED_TOKENS = Value("SWAPPED_TOKENS")
  final val TOKEN_REQUIRED = Value("TOKEN_REQUIRED")
  final val TWO_STATEMENTS = Value("TWO_STATEMENTS")  // NEW 18-JAN-2020
  final val TUPLE_NEEDS_PARENS = Value("TUPLE_NEEDS_PARENS")
  final val UNEXPECTED_END_OF_INPUT = Value("UNEXPECTED_END_OF_INPUT")
  final val UNEXPECTED_KEYWORD = Value("UNEXPECTED_KEYWORD")
  final val UNMATCHED_BRACKET = Value("UNMATCHED_BRACKET")
  final val UNREACHABLE_CODE = Value("UNREACHABLE_CODE")
  final val UNTERMINATED_STRING = Value("UNTERMINATED_STRING")
  final val USE_AND_NOT_COMMA = Value("USE_AND_NOT_COMMA")
  final val USE_BREAK_INSTEAD_OF_RETURN = Value("BREAK_INSTEAD_OF_RETURN")
  final val USE_COMMA_NOT_AND = Value("USE_COMMA_NOT_AND")
  final val USE_ELIF_INSTEAD_OF_ELSE = Value("USE_ELIF_INSTEAD_OF_ELSE")
  final val USE_ELIF_INSTEAD_OF_ELSE_IF = Value("USE_ELIF_INSTEAD_OF_ELSE_IF")
  final val USE_EQ_INSTEAD_OF_NEQ = Value("USE_EQ_INSTEAD_OF_NEQ")
  final val USE_MOD_NOT_DIV = Value("USE_MOD_NOT_DIV")
  final val USE_NOT_INSTEAD_OF_FALSE = Value("USE_NOT_INSTEAD_OF_FALSE")
  final val USE_PYTHON_POWER = Value("USE_PYTHON_POWER")
  final val USE_REPEAT_INSTEAD_OF_WHILE = Value("USE_REPEAT_INSTEAD_OF_WHILE")
  final val USE_RETURN_INSTEAD_OF_BREAK = Value("RETURN_INSTEAD_OF_BREAK")
  final val USE_SEMICOLON_INSTEAD_OF_COMMA = Value("USE_SEMICOLON_INSTEAD_OF_COMMA")
  final val USELESS_COMPUTATION = Value("USELESS_COMPUTATION")
  final val USELESS_STATEMENT = Value("USELESS_STATEMENT")
  final val USELESS_STMT_USE_AUG_ASSIGN = Value("USELESS_STMT_USE_AUG_ASSIGN")
  final val VARARG_AFTER_KEYWORD_ARG = Value("VARARG_AFTER_KEYWORD_ARG")
  final val VARARG_NOT_ALLOWED = Value("VARARG_NOT_ALLOWED")
  final val WRONG_BRACKET = Value("WRONG_BRACKET")
  final val WRONG_TOKEN = Value("WRONG_TOKEN")
  final val YIELD_OUTSIDE_FUNCTION = Value("YIELD_OUTSIDE_FUNCTION")
}
