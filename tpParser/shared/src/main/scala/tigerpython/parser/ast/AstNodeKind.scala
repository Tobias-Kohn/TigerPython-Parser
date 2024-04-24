package tigerpython.parser.ast

/**
  * These values represent the kinds of AST nodes used in CPython as good as possible.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 01/03/2020
  * Updated by Tobias Kohn on 24/04/2024
  */
object AstNodeKind extends Enumeration {

  final val MODULE = Value("Module")
  final val INTERACTIVE = Value("Interactive")
  final val EXPRESSION = Value("Expression")
  final val FUNCTION_TYPE = Value("FunctionType")
  final val SUITE = Value("Suite")

  final val FUNCTION_DEF = Value("FunctionDef")
  final val ASYNC_FUNCTION_DEF = Value("AsyncFunctionDef")
  final val CLASS_DEF = Value("ClassDef")
  final val RETURN = Value("Return")
  final val DELETE = Value("Delete")
  final val ASSIGN = Value("Assign")
  final val AUG_ASSIGN = Value("AugAssign")
  final val ANN_ASSIGN = Value("AnnAssign")
  final val FOR = Value("For")
  final val ASYNC_FOR = Value("AsyncFor")
  final val WHILE = Value("While")
  final val IF = Value("If")
  final val WITH = Value("With")
  final val ASYNC_WITH = Value("AsyncWith")
  final val RAISE = Value("Raise")
  final val TRY = Value("Try")
  final val ASSERT = Value("Assert")
  final val IMPORT = Value("Import")
  final val IMPORT_FROM = Value("ImportFrom")
  final val GLOBAL = Value("Global")
  final val NON_LOCAL = Value("Nonlocal")
  final val EXPR = Value("Expr")
  final val PASS = Value("Pass")
  final val BREAK = Value("Break")
  final val CONTINUE = Value("Continue")

  final val BOOL_OP = Value("BoolOp")
  final val NAMED_EXPR = Value("NamedExpr")
  final val BIN_OP = Value("BinOp")
  final val UNARY_OP = Value("UnaryOp")
  final val LAMBDA = Value("Lambda")
  final val IF_EXPR = Value("IfExpr")
  final val DICT = Value("Dict")
  final val SET = Value("Set")
  final val LIST_COMP = Value("ListComp")
  final val SET_COMP = Value("SetComp")
  final val DICT_COMP = Value("DictComp")
  final val GENERATOR_EXPR = Value("GeneratorExpr")
  final val AWAIT = Value("Await")
  final val YIELD = Value("Yield")
  final val YIELD_FROM = Value("YieldFrom")
  final val COMPARE = Value("Compare")
  final val CALL = Value("Call")
  final val FORMATTED_VALUE = Value("FormattedValue")
  final val JOINED_STR = Value("JoinedStr")
  final val CONSTANT = Value("Constant")
  final val ATTRIBUTE = Value("Attribute")
  final val SUBSCRIPT = Value("Subscript")
  final val STARRED = Value("Starred")
  final val NAME = Value("Name")
  final val LIST = Value("List")
  final val TUPLE = Value("Tuple")

  final val MATCH = Value("MATCH")
  final val MATCH_CASE = Value("MATCH_CASE")
  final val PATTERN = Value("PATTERN")

  final val SLICE = Value("Slice")
  final val EXT_SLICE = Value("ExtSlice")
  final val INDEX = Value("Index")

  final val EXCEPT_HANDLER = Value("ExceptHandler")

  final val ALIAS = Value("alias")
  final val ARGUMENT = Value("<arg>")
  final val ARGUMENTS = Value("<arguments>")
  final val COMPREHENSION = Value("<comprehension>")
  final val PARAMETER = Value("arg")
  final val PARAMETERS = Value("arguments")

  final val EXEC_2 = Value("Exec/2")
  final val PRINT_2 = Value("Print/2")
  final val RAISE_2 = Value("Raise/2")
  final val NOTHING = Value("<NOTHING>")
}
