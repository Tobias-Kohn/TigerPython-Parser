package tigerpython.utilities.completer

import tigerpython.utilities.fastparse._
import tigerpython.utilities.types._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * This takes a Pyi-file and defines a TigerPython-module from it.
 */
class PyiModuleParser(val module: Module, val moduleLookup: mutable.Map[String, DataType]) extends PyiParser {

  // Maps an alias to a module, imported either as:
  // import math          # produces "math" -> <math Module>
  // import math as m     # produces "m" -> <math Module>
  private val fullModuleImports: mutable.Map[String, Module] = mutable.Map()

  private var currentClass: PythonClass = _

  /**
   * This method takes an Expr-AST-element from the PYI-parser and converts it into one of our internal datatypes.  If
   * the type is not properly recognised, `ANY_TYPE` is returned as fallback.  In fact, we only support a small subset
   * of what is possible in Python.
   */
  protected
  def convertToType(pyiType: ExprAst): DataType =
    pyiType match {
      case NameNode(name) =>
        module.findField(name) match {
          case Some(tp) =>
            tp
          case _ =>
            BuiltinTypes.fromString(name)
        }
      case SubscriptNode(NameNode("list" | "List"), subscript) =>
        ListType(convertToType(subscript))
      case SubscriptNode(NameNode("tuple" | "Tuple"), TupleNode(elts)) =>
        TupleType(for (el <- elts) yield convertToType(el))
      case OrNode(elts) if elts.nonEmpty =>
        var tp = convertToType(elts.head)
        for (el <- elts.tail)
          tp = DataType.getCompatibleType(tp, convertToType(el))
        tp
      case AttributeNode(base, name) =>
        val baseDotted = PyiModuleParser.toDotted(base)
        if (baseDotted != null && fullModuleImports.contains(baseDotted))
          fullModuleImports(baseDotted).findField(name) match {
            case Some (tp) =>
              tp
            case _ =>
              BuiltinTypes.ANY_TYPE
          }
        else
          BuiltinTypes.ANY_TYPE
      case _ =>
        BuiltinTypes.ANY_TYPE
    }

  private
  def hasDecorator(decorator: ExprAst, name: String): Boolean =
    decorator match {
      case NameNode(n) =>
        name == n
      case TupleNode(elts) =>
        elts.exists(x => hasDecorator(x, name))
      case _ =>
        false
    }

  private def convertBaseClasses(baseClasses: Array[ExprAst]): Array[ClassType] =
    if (baseClasses != null && baseClasses.nonEmpty) {
      val result = ArrayBuffer[ClassType]()
      for (base <- baseClasses)
        convertToType(base) match {
          case cls: ClassType =>
            result += cls
          case _ =>
        }
      result.toArray
    } else
      Array.empty[ClassType]

  override protected
  def defineClass(className: String, baseClasses: Array[ExprAst], metaClass: ExprAst): Unit = {
    val cls = new PythonClass(className, convertBaseClasses(baseClasses))
    currentClass = cls
    module.setField(cls.name, cls)
  }

  override protected
  def defineClassDocString(className: String, doc: String): Unit = {
    if (currentClass != null && currentClass.name == className)
      currentClass.docString = doc
  }

  override protected
  def defineFunction(functionName: String, arguments: FunctionArguments, returnType: ExprAst,
                     doc: String, className: String, decorator: ExprAst, isAsync: Boolean): Unit = {
    val params = new ArrayBuffer[Parameter]()
    for (arg <- arguments.posOnlyArguments)
      params += Parameter(arg.name, convertToType(arg.argType))
    for (arg <- arguments.arguments)
      params += Parameter(arg.name, convertToType(arg.argType))
    val paramCount = (arguments.posOnlyArguments.count(_.defaultValue == null) +
      arguments.arguments.count(_.defaultValue == null))
    if (className != null && currentClass != null && currentClass.name == className &&
        params.nonEmpty && !hasDecorator(decorator, "staticmethod")) {
      if (hasDecorator(decorator, "classmethod"))
        params.head.dataType = currentClass
      else
        params.head.dataType = new SelfInstance(currentClass)
    }
    val f = new PythonFunction(functionName, params.toArray, paramCount, convertToType(returnType))
    if (doc != null)
      f.docString = doc
    if (className == null)
      module.setField(f.name, f)
    else if (currentClass != null && currentClass.name == className)
      currentClass.setField(f.name, f)
  }

  override protected
  def defineVariable(varName: String, varType: ExprAst, varValue: ExprAst, className: String): Unit = {
    val tp = convertToType(varType)
    if (className == null)
      module.setField(varName, tp)
    else if (currentClass != null && currentClass.name == className)
      currentClass.setField(varName, tp)
  }

  override protected
  def importModule(module: String, alias: String): Boolean = {
    moduleLookup.get(module) match {
      case Some(m : Module) if m != null =>
        if (alias == null)
          fullModuleImports(module) = m;
        else
          fullModuleImports(alias) = m;
    }
    false
  }

  override protected
  def importNameFromModule(module: String, name: String, alias: String): Boolean = false
}

object PyiModuleParser {
  private def toDotted(exprAst: ExprAst): String = {
    exprAst match {
      case NameNode(name) =>
        name
      case AttributeNode(base, name) =>
        val prefix = toDotted(base)
        if (prefix == null) null
        else s"$prefix.$name"
      case _ =>
        null
    }
  }
}
