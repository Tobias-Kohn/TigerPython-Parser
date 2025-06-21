package tigerpython.utilities
package scopes

import tigerpython.parser.ast._
import tigerpython.utilities.types.BuiltinTypes.STRING_TYPE
import types._

import scala.collection.mutable.ListBuffer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 15.10.2017.
  */
class AstWalker(val scope: Scope) {

  import BuiltinTypes.ANY_TYPE

  protected val typeWalker: types.TypeAstWalker = new types.TypeAstWalker() {

    override def findName(name: String): Option[DataType] = {
      val result = super.findName(name)
      if (result.isDefined)
        result
      else
        scope.findLocal(name)
    }

    override def getCurrentClass: Option[ClassType] =
      scope.getCurrentClass match {
        case Some(classScope) =>
          Some(classScope.pyClass)
        case _ =>
          None
      }
  }

  private def getType(expr: AstNode.Expression): DataType = typeWalker.getType(expr)

  private def getElementType(expr: AstNode.Expression): DataType =
    expr match {
      case seq: AstNode.SequenceExpression if seq.elements.nonEmpty =>
        val result = getType(seq.elements.head)
        if (seq.elements.forall(x => getType(x) == result))
          result
        else
          BuiltinTypes.ANY_TYPE
      case _ =>
        val tp = getType(expr)
        if (tp != null && tp != BuiltinTypes.ANY_TYPE)
          tp.getItemType
        else
          BuiltinTypes.ANY_TYPE
    }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private final def getAttributeBase(node: AstNode.Expression): Option[DataType] =
    node match {
      case AstNode.Attribute(_, _, base, attr) =>
        getAttributeBase(base) match {
          case Some(b) =>
            b.findField(attr.name)
          case None =>
            None
        }
      case AstNode.Name(_, name) =>
        findName(name)
      case _ =>
        None
    }

  private final def getStringValue(node: AstNode.Expression): String =
    node match {
      case s: AstNode.StringValue =>
        s.value
      case attr: AstNode.Attribute if attr.getAttributeName == "__doc__" =>
        getAttributeBase(attr.base) match {
          case Some(b) =>
            b.docString
          case _ =>
            null
        }
      case binOp: AstNode.BinaryOp =>
        binOp.op match {
          case BinOp.ADD =>
            val s1 = getStringValue(binOp.left)
            val s2 = getStringValue(binOp.right)
            if (s1 != null && s2 != null)
              return s1 + s2
          case BinOp.MUL =>
            val s1 = getStringValue(binOp.left)
            if (s1 != null)
              binOp.right match {
                case r @ AstNode.Value(_, ValueType.INTEGER) =>
                  return s1 * r.value.toInt
                case _ =>
              }
          case _ =>
        }
        null
      case _ =>
        null
    }

  protected def defineName(dest: AstNode.Expression, dataType: DataType): Unit =
    dest match {
      case AstNode.Name(_, target) =>
        scope.define(target, dataType)
      case AstNode.Attribute(_, _, base, attr) =>
        getAttributeBase(base) match {
          case Some(b) =>
            b.setField(attr.name, dataType)
          case None =>
        }
      case _ =>
    }

  protected def findName(name: String): Option[DataType] =
    if (scope != null)
      scope.findLocal(name)
    else
      Scope.findGlobal(name)

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def walkNode(node: AstNode): Unit =
    node match {
      case assignment: AstNode.Assignment =>
        walkAssignment(assignment)
      case classDef: AstNode.ClassDef =>
        walkClass(classDef)
      case expr: AstNode.ExprStatement =>
        getType(expr.expression)
      case forStmt: AstNode.For =>
        walkFor(forStmt)
      case funDef: AstNode.FunctionDef =>
        walkFunction(funDef)
      case global: AstNode.Global =>
        walkGlobal(global)
      case ifStmt: AstNode.If =>
        walkIf(ifStmt)
      case importStmt: AstNode.Import =>
        walkImport(importStmt)
      case importStmt: AstNode.ImportFrom =>
        walkImportFrom(importStmt)
      case importStmt: AstNode.ImportStar =>
        walkImportAll(importStmt)
      case ret: AstNode.Return =>
        val dt = ret.value match {
          case tuple: AstNode.Tuple if tuple.elements.length <= 3 =>
            val itemTypes = new Array[DataType](tuple.elements.length)
            for (i <- itemTypes.indices)
              itemTypes(i) = getType(tuple.elements(i))
            if (!itemTypes.forall(_ == ANY_TYPE))
              TupleType(itemTypes)
            else
              BuiltinTypes.TUPLE
          case null =>
            BuiltinTypes.NONE_TYPE
          case value =>
            getType(value)
        }
        scope.returnType = dt
      case suite: AstNode.Suite =>
        walkSuite(suite)
      case tryStmt: AstNode.Try =>
        walkNode(tryStmt.body)
        for (handler <- tryStmt.handlers)
          walkNode(handler.body)
        walkNode(tryStmt.elseBody)
        walkNode(tryStmt.finalBody)
      case withStmt: AstNode.With =>
        if (withStmt.opt_vars != null)
          walkWith(withStmt)
      case bodyStmt: AstNode.Body =>
        walkNode(bodyStmt.body)
        walkNode(bodyStmt.elseBody)
      case _ =>
    }

  protected def walkAssignment(assignment: AstNode.Assignment): Unit =
    if (!assignment.value.isInstanceOf[AstNode.EmptyExpression])
      for (target <- assignment.targets)
        target match {
          case _: AstNode.Name =>
            defineName(target, getType(assignment.value))
          case target: AstNode.Tuple =>
            assignment.value match {
              case tuple: AstNode.Tuple if tuple.length == target.length =>
                for (i <- target.elements.indices)
                  defineName(target.elements(i), getType(tuple.elements(i)))
              case call: AstNode.Call =>
                getType(call) match {
                  case tuple: TupleType if tuple.length == target.length =>
                    for (i <- target.elements.indices)
                      defineName(target.elements(i), tuple.itemTypes(i))
                  case _ =>
                    for (t <- target.elements)
                      defineName(t, BuiltinTypes.ANY_TYPE)
                }
              case _ =>
                for (t <- target.elements)
                  defineName(t, BuiltinTypes.ANY_TYPE)
            }
          case attr: AstNode.Attribute =>
            if (attr.getAttributeName == "__doc__")
              getAttributeBase(attr.base) match {
                case Some(b: PythonFunction) =>
                  b.docString = getStringValue(assignment.value)
                  return
                case Some(b: SelfInstance) =>
                  b.docString = getStringValue(assignment.value)
                  return
                case Some(b: PythonClass) =>
                  b.docString = getStringValue(assignment.value)
                  return
                case _ =>
              }
            defineName(attr, getType(assignment.value))
          case _ =>
        }

  protected def walkClass(cls: AstNode.ClassDef): Unit = {
    val bases = (for (b <- cls.bases) yield getType(b))
      .filter(_.isInstanceOf[ClassType])
      .map(_.asInstanceOf[ClassType])
    val result = new PythonClass(cls.getName, bases)
    result.docString = cls.docString
    scope match {
      case cls: ClassScope =>
        cls.pyClass.setField(result.name, result)
      case mod: ModuleScope =>
        mod.module.setField(result.name, result)
      case _ =>
    }
    val classScope = new ClassScope(cls.pos, cls.endPos, result)
    scope.addScope(classScope)
    result.source = classScope.getCurrentPath
    result.sourcePos = cls.pos
    val walker = new AstWalker(classScope)
    walker.walkNode(cls.body)
  }

  protected def walkFunction(function: AstNode.FunctionDef): Unit = {
    val params = getParameters(function.params)
    if (params.nonEmpty)
      scope match {
        case cls: ClassScope if !function.hasDecorator("staticmethod") =>
          if (function.hasDecorator("classmethod"))
            params(0).dataType = new SelfClass(cls.pyClass)
          else
            params(0).dataType = new SelfInstance(cls.pyClass)
          if (params.length > 1 && params(1).dataType == ANY_TYPE)
            params(1).dataType = BuiltinTypes.ECHO_TYPE
          if (params.length > 2 && params(2).dataType == ANY_TYPE)
            params(2).dataType = BuiltinTypes.ECHO2_TYPE
        case _ =>
          if (params.length > 0 && params(0).dataType == ANY_TYPE)
            params(0).dataType = BuiltinTypes.ECHO_TYPE
          if (params.length > 1 && params(1).dataType == ANY_TYPE)
            params(1).dataType = BuiltinTypes.ECHO2_TYPE
      }
    val result = new PythonFunction(function.name.name, params,
      function.params.maxPositionalArgCount min params.length, getSignature(function.params, ANY_TYPE, params.length > 0 && params(0).dataType.isInstanceOf[SelfInstance]), ANY_TYPE)
    result.docString = function.docString
    scope match {
      case cls: ClassScope =>
        if (function.hasDecorator("classmethod", "staticmethod"))
          cls.pyClass.setField(result.name, result)
        cls.pyClass.setInstanceField(result.name, result)
      case mod: ModuleScope =>
        mod.module.setField(result.name, result)
      case _ =>
    }
    val functionScope = new FunctionScope(function.pos, function.endPos, result)
    scope.addScope(functionScope)
    result.source = functionScope.getCurrentPath
    result.sourcePos = function.pos
    val walker = new AstWalker(functionScope)
    walker.walkNode(function.body)
    if (!hasReturn(function.body))
      functionScope.returnType = BuiltinTypes.NONE_TYPE
    result.returnType = functionScope.returnType
  }

  protected def walkFor(forStmt: AstNode.For): Unit =
    forStmt.target match {
      case AstNode.Name(_, name) =>
        _walkFor(forStmt, Map(name -> getType(forStmt.iter).getItemType))
      case tuple: AstNode.Tuple if tuple.elements.forall(_.isInstanceOf[AstNode.Name]) =>
        val params = collection.mutable.Map[String, DataType]()
        for (elem <- tuple.elements)
          params(elem.asInstanceOf[AstNode.Name].name) = BuiltinTypes.ANY_TYPE
        _walkFor(forStmt, params.toMap)
      case _ =>
        walkNode(forStmt.body)
        walkNode(forStmt.elseBody)
    }

  private def _walkFor(forStmt: AstNode.For, params: Map[String, DataType]): Unit = {
    val forScope = new ForLoopScope(forStmt.pos, forStmt.endPos, params)
    scope.addScope(forScope)
    val walker = new AstWalker(forScope)
    walker.walkNode(forStmt.body)
    walkNode(forStmt.elseBody)
  }

  protected def walkWith(withStmt: AstNode.With): Unit =
    withStmt.opt_vars match {
      case AstNode.Name(_, name) =>
        val ctx = getType(withStmt.context)
        val context =
          if (ctx.isCallable)
            Instance(ctx.getReturnType)
          else
            ctx
        context.findField("__enter__") match {
          case Some(enter) if enter.isCallable =>
            _walkWith(withStmt, Map(name -> Instance(enter.getReturnType)))
          case _ =>
            _walkWith(withStmt, Map(name -> context))
        }
      case tuple: AstNode.Tuple if tuple.elements.forall(_.isInstanceOf[AstNode.Name]) =>
        val params = collection.mutable.Map[String, DataType]()
        for (elem <- tuple.elements)
          params(elem.asInstanceOf[AstNode.Name].name) = BuiltinTypes.ANY_TYPE
        _walkWith(withStmt, params.toMap)
      case _ =>
        walkNode(withStmt.body)
    }

  private def _walkWith(withStmt: AstNode.With, params: Map[String, DataType]): Unit = {
    val withScope = new ForLoopScope(withStmt.pos, withStmt.endPos, params)
    scope.addScope(withScope)
    val walker = new AstWalker(withScope)
    walker.walkNode(withStmt.body)
  }

  protected def walkIf(ifStmt: AstNode.If): Unit = {
    ifStmt.test match {
      case call: AstNode.Call if call.getFunctionName == "isinstance" && call.args.length == 2 =>
        val n = call.getFirstArgAsName
        getType(call.args(1)) match {
          case cls: ClassType if n != null =>
            _walkIf(ifStmt, Map(n -> Instance(cls)))
            walkNode(ifStmt.elseBody)
            return
          case _ =>
        }
      case cmp: AstNode.Compare if cmp.isSimpleEqual || cmp.isRefEqualTest =>
        cmp.left match {
          case call: AstNode.Call if call.getFunctionName == "type" =>
            val n = call.getFirstArgAsName
            getType(cmp.right) match {
              case cls: ClassType if n != null =>
                _walkIf(ifStmt, Map(n -> Instance(cls)))
                walkNode(ifStmt.elseBody)
                return
              case _ =>
            }
          case _ =>
        }
      case cmp: AstNode.Compare if cmp.isInclusionTest =>
        cmp.left match {
          case call: AstNode.Call if call.getFunctionName == "type" =>
            val n = call.getFirstArgAsName
            cmp.right match {
              case seq: AstNode.SequenceExpression if seq.elements.nonEmpty =>
                var result = getType(seq.elements.head)
                for (elem <- seq.elements.tail)
                  result = DataType.getCompatibleType(result, getType(elem))
                result match {
                  case cls: ClassType if n != null =>
                    _walkIf(ifStmt, Map(n -> Instance(cls)))
                    walkNode(ifStmt.elseBody)
                    return
                  case _ =>
                }
              case _ =>
            }
          case name: AstNode.Name =>
            _walkIf(ifStmt, Map(name.name -> getElementType(cmp.right)))
            walkNode(ifStmt.elseBody)
            return
          case _ =>
        }
      case _  =>
    }
    walkNode(ifStmt.body)
    walkNode(ifStmt.elseBody)
  }

  private def _walkIf(ifStmt: AstNode.If, params: Map[String, DataType]): Unit = {
    val ifScope = new ForLoopScope(ifStmt.pos, ifStmt.elsePos, params)
    scope.addScope(ifScope)
    val walker = new AstWalker(ifScope)
    walker.walkNode(ifStmt.body)
  }

  protected def walkGlobal(global: AstNode.Global): Unit =
    scope match {
      case function: FunctionScope =>
        for (name <- global.names)
          function.addGlobal(name.name)
      case forLoop: ForLoopScope =>
        for (name <- global.names)
          forLoop.addGlobal(name.name)
      case _ =>
    }

  protected def walkImport(importStmt: AstNode.Import): Unit =
    for (alias <- importStmt.names;
         name = alias.name.name.filter(_ != '?'))
      scope.importModule(name, null) match {
        case Some(_) =>
          val impModule = scope.importModule(name, null).get
          if (alias.asName != null)
            scope.define(alias.asName.name, impModule)
          else
            scope.define(impModule)
        case None =>
      }

  protected def walkImportFrom(importStmt: AstNode.ImportFrom): Unit = {
    val module = importStmt.module.name.filter(_ != '?')
    for (alias <- importStmt.names)
      scope.importModule(module, alias.name.name) match {
        case Some(impModule) =>
          if (alias.asName != null)
            scope.define(alias.asName.name, impModule)
          else
            scope.define(impModule)
        case None =>
      }
  }

  protected def walkImportAll(importStmt: AstNode.ImportStar): Unit =
    scope.importModule(importStmt.module.name.filter(_ != '?'), null) match {
      case Some(impModule) =>
        scope.loadFrom(impModule)
      case None =>
    }

  private def hasReturn(stmt: AstNode.Statement): Boolean =
    stmt match {
      case _: AstNode.Return =>
        true
      case _: AstNode.Raise2 | _: AstNode.Raise3 =>
        true
      case ifStmt: AstNode.If =>
        hasReturn(ifStmt.body) && hasReturn(ifStmt.elseBody)
      case tryStmt: AstNode.Try =>
        if (tryStmt.finalBody != null)
          hasReturn(tryStmt.finalBody)
        else if (tryStmt.elseBody != null)
          hasReturn(tryStmt.elseBody)
        else
          hasReturn(tryStmt.body)
      case suite: AstNode.Suite =>
        if (suite.statements.nonEmpty)
          hasReturn(suite.statements.last)
        else
          false
      case _ =>
        false
    }

  protected def walkSuite(suite: AstNode.Suite): Unit = {
    for (stmt <- suite.statements)
      walkNode(stmt)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected def getParameters(params: AstNode.Parameters): Array[Parameter] =
    if (params != null && params.args != null && params.defaults != null) {
      val delta = params.args.length - params.defaults.length
      val result = collection.mutable.ArrayBuffer[Parameter]()
      for (i <- params.args.indices)
        params.args(i) match {
          case AstNode.NameParameter(_, name, annotation) =>
            val dataType =
              if (annotation != null)
                getType(annotation)
              else if (i >= delta) {
                getType(params.defaults(i - delta)._1) match {
                  case BuiltinTypes.NONE_TYPE => BuiltinTypes.ANY_TYPE
                  case d => d
                }
              } else
                BuiltinTypes.ANY_TYPE
            result += Parameter(name, dataType)
          case _ =>
        }
      if (params.varArgs != null)
        result += Parameter(params.varArgs.name, BuiltinTypes.TUPLE_TYPE)
      if (params.kwArgs != null)
        result += Parameter(params.kwArgs.name, BuiltinTypes.DICT_TYPE)
      result.toArray
    } else
      Array()

  protected def getSignature(params: AstNode.Parameters, returnType: DataType, firstParamIsSelf: Boolean): Signature =
    if (params != null && params.args != null && params.defaults != null) {
      val positionalOnlyArgs: ListBuffer[SignatureArg] = ListBuffer()
      val positionalOrKeywordArgs: ListBuffer[SignatureArg] = ListBuffer()
      var varArgs: Option[SignatureVarArg] = None           // *args
      val keywordOnlyArgs: ListBuffer[SignatureArg] = ListBuffer()     // After *
      var varKwargs: Option[SignatureVarArg] = None
      // Defaults are from the end backwards, so the ends line up, not the starts:
      val delta = params.args.length - params.defaults.length
      for (i <- params.args.indices)
        params.args(i) match {
          case AstNode.NameParameter(_, name, annotation) =>
            val dataType =
              if (annotation != null)
                getType(annotation)
              else if (i >= delta) {
                getType(params.defaults(i - delta)._1) match {
                  case BuiltinTypes.NONE_TYPE => BuiltinTypes.ANY_TYPE
                  case d => d
                }
              } else
                BuiltinTypes.ANY_TYPE
            val addTo =
              if (i < params.maxPositionalOnlyArgCount)
                positionalOnlyArgs
              else if (i < params.maxPositionalArgCount)
                positionalOrKeywordArgs
              else
                keywordOnlyArgs
            addTo += SignatureArg(name, Option(if (i - delta >= 0) params.defaults(i - delta)._2 else null), dataType)
          case _ =>
        }
      if (params.varArgs != null)
        varArgs = Option(SignatureVarArg(params.varArgs.name, if (params.varArgs.annotation != null) new VarTupleType(getType(params.varArgs.annotation)) else BuiltinTypes.TUPLE_TYPE))
      if (params.kwArgs != null)
        varKwargs = Option(SignatureVarArg(params.kwArgs.name, if (params.kwArgs.annotation != null) new DictType(STRING_TYPE, getType(params.kwArgs.annotation)) else BuiltinTypes.DICT_TYPE))
      Signature(positionalOnlyArgs.result(), positionalOrKeywordArgs.result(), varArgs, keywordOnlyArgs.result(), varKwargs, returnType, firstParamIsSelf)
    } else
      null



  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
