package tigerpython.utilities
package scopes

import tigerpython.parser.ast.AstNode
import types.{ClassType, DataType, Instance, ListType, Module}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 15.10.2017.
  */
abstract class Scope {
  var parent: Scope = _
  def startPos: Int
  def endPos: Int

  protected val subScopes: collection.mutable.ArrayBuffer[Scope] = collection.mutable.ArrayBuffer[Scope]()

  def addScope(scope: Scope): Scope =
    if (scope != null) {
      subScopes += scope
      scope.parent = this
      scope
    } else
      null

  // Replaces a previously-added sub-scope in place (same array slot), so that
  // findScope keeps resolving to the up-to-date scope instead of a stale one that
  // was walked before its function's parameter types were refined from call-site
  // evidence. Falls back to appending if oldScope isn't a current sub-scope.
  def replaceScope(oldScope: Scope, newScope: Scope): Unit =
    if (newScope != null) {
      val idx = subScopes.indexOf(oldScope)
      if (idx >= 0)
        subScopes(idx) = newScope
      else
        subScopes += newScope
      newScope.parent = this
    }

  def findScope(position: Int): Option[Scope] =
    if (endPos == -1 || (startPos <= position && position <= endPos)) {
      for (scope <- subScopes) {
        val result = scope.findScope(position)
        if (result.isDefined)
          return result
      }
      Some(this)
    } else
      None

  def getCurrentClass: Option[ClassScope] =
    if (parent != null)
      parent.getCurrentClass
    else
      None

  def getCurrentPath: String =
    if (parent != null)
      parent.getCurrentPath
    else
      ""

  def getModule: ModuleScope =
    if (parent != null)
      parent.getModule
    else
      null

  def define(dataType: DataType): Unit =
    if (dataType != null)
      define(dataType.name, dataType)

  def define(name: String, dataType: DataType): Unit

  private lazy val typeAstWalker: types.TypeAstWalker = new types.TypeAstWalker() {
    override def findName(name: String): Option[DataType] = {
      val result = super.findName(name)
      if (result.isDefined) result else findLocal(name)
    }

    override def getCurrentClass: Option[ClassType] =
      Scope.this.getCurrentClass.map(_.pyClass)
  }

  def findLocal(name: String): Option[DataType] =
    getLocals.get(name) match {
      case None =>
        if (parent != null)
          parent.findLocal(name)
        else
          Scope.findGlobal(name)
      case result =>
        result
    }

  def findName(name: String): Option[DataType] =
    if (name.contains('.')) {
      val idx = name.lastIndexOf('.')
      findName(name.take(idx)) match {
        case Some(dt) =>
          dt.findField(name.drop(idx+1))
        case None =>
          None
      }
    } else
      findLocal(name)

  def findName(ast: AstNode): Option[DataType] =
    ast match {
      case attr: AstNode.Attribute =>
        findName(attr.base) match {
          case Some(dt) =>
            dt.findField(attr.attr.name)
          case None =>
            None
        }
      case call: AstNode.Call =>
        // Delegate to the full TypeAstWalker rather than re-deriving the return type here: some
        // builtins (sorted/max/min/property/...) rely on argument-dependent ECHO_* sentinel return
        // types (see BuiltinTypes), which a plain `Instance(dt.getReturnType)` can't express.
        Some(typeAstWalker.getType(call))
      case subscript: AstNode.Subscript =>
        findName(subscript.base) match {
          case Some(dt) =>
            subscript.slice match {
              case _: AstNode.Index =>
                Some(dt.getItemType)
              case _ =>
                Some(dt)
            }
          case _ =>
            None
        }
      case name: AstNode.Name =>
        findLocal(name.name)
      case _: AstNode.Dict | _: AstNode.DictComp =>
        Some(types.BuiltinTypes.LIST)
      case lst: AstNode.List if lst.elements.nonEmpty =>
        val itemTypes = for (el <- lst.elements) yield findName(el).orNull
        val itemType = itemTypes.head
        if (itemType != null && itemTypes.forall(_ == itemType))
          Some(new Instance(ListType(itemType)))
        else
          Some(types.BuiltinTypes.LIST)
      case _: AstNode.List | _: AstNode.ListComp =>
        Some(types.BuiltinTypes.LIST)
      case _: AstNode.StringValue =>
        Some(types.BuiltinTypes.STRING)
      case expr: AstNode.Expression =>
        // Catch-all for literal/expression kinds with no bespoke case above (int/float/complex/none
        // literals, booleans, unary/binary ops, comparisons, ...): TypeAstWalker.getType already
        // knows how to resolve these, so delegate rather than falling through to None, which would
        // make e.g. `(5).bit_length` unresolvable and fall back to a full builtin-name dump.
        Some(typeAstWalker.getType(expr))
      case _ =>
        None
    }

  private def _findName(base: Option[DataType], ast: AstNode): Option[DataType] =
    if (base.isDefined)
      ast match {
        case attr: AstNode.Attribute =>
          _findName(_findName(base, attr.base), attr.attr)
        case name: AstNode.Name =>
          base.get.findField(name.name)
        case _ =>
          None
      }
    else
      None

  def findName(baseAst: AstNode, ast: AstNode): Option[DataType] =
    _findName(findName(baseAst), ast)

  def getAllLocals: Map[String, DataType] =
    if (parent != null)
      parent.getAllLocals ++ getLocals
    else
      getLocals

  def getLocals: Map[String, DataType]

  def isLocal(name: String): Boolean

  def incNameUseCounter(name: AstNode.Name): Unit = {}

  def loadFrom(source: DataType): Unit =
    for ((name, field) <- source.getFields)
      if (!name.startsWith("_"))
        define(name, field)

  def returnType: DataType =
    if (parent != null)
      parent.returnType
    else
      null

  def returnType_=(retType: DataType): Unit =
    if (parent != null)
      parent.returnType = retType

  def importModule(moduleName: String, importName: String): Option[DataType] =
    if (parent != null)
      parent.importModule(moduleName, importName)
    else if (importName == null || importName == "")
      Some(ModuleLoader.defaultModuleLoader.importName(moduleName))
    else
      Some(ModuleLoader.defaultModuleLoader.importNameFrom(moduleName, importName))

  override def toString: String = {
    val head = "%s(%d, %d)".format(getClass.getSimpleName, startPos, endPos)
    val scopes = for (scope <- subScopes)
      yield scope.toString.replace("\n", "\n  ")
    val top = if (scopes.nonEmpty)
      "%s\n  %s".format(head, scopes.mkString("\n  "))
    else
      head
    val localNames = for ((name, dataType) <- getLocals)
      yield "%s = %s".format(name, dataType.toString)
    if (localNames.nonEmpty)
      "%s\n  %s".format(top, localNames.mkString("\n  "))
    else
      top
  }
}
object Scope {
  def findGlobal(name: String): Option[DataType] = getGlobals.get(name)

  def fromAst(moduleName: String, ast: AstNode,
              moduleLoader: ModuleLoader = ModuleLoader.defaultModuleLoader): ModuleScope = {
    val module = new Module(moduleName)
    val moduleScope = new ModuleScope(-1, module, moduleLoader)
    if (ast != null) {
      val walker = new AstWalker(moduleScope)
      walker.walkNode(ast)
      if (moduleScope.inferableFunctionDefs.nonEmpty)
        walker.reinferParamsFromCallSites(moduleScope)
    }
    moduleScope
  }

  def getGlobals: Map[String, DataType] = BuiltinNames.getGlobals
}
