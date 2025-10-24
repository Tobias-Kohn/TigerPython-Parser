package tigerpython.utilities
package types

import tigerpython.parser.ast.AstNode.{Index, MultiSlice}
import tigerpython.parser.ast._

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 29.06.2016.
  */
class TypeAstWalker {

  import BuiltinTypes.ANY_TYPE

  protected val paramStack = collection.mutable.Stack[TypeAstWalker.Parameter]()

  def findName(name: String): Option[DataType] =
    paramStack.find(_.name == name) match {
      case Some(param) =>
        Some(param.dataType)
      case _ =>
        None
    }

  def getCurrentClass: Option[ClassType] = None

  @inline
  private final def validateDataType(dataType: DataType): DataType =
    if (dataType != null) dataType else ANY_TYPE

  @inline
  private final def validateDataType(dataType: Option[DataType]): DataType =
    if (dataType.isDefined) dataType.get else ANY_TYPE

  def getType(expr: AstNode.Expression): DataType =
    expr match {
      case _: AstNode.StringValue =>
        BuiltinTypes.STRING
      case _: AstNode.BooleanValue =>
        BuiltinTypes.BOOLEAN
      case _: AstNode.Compare =>
        BuiltinTypes.BOOLEAN
      case attr: AstNode.Attribute =>
        getTypeOfAttr(attr)
      case await: AstNode.Await =>
        getType(await.expr)
      case binOp: AstNode.BinaryOp =>
        getTypeOfBinaryOp(binOp)
      case call: AstNode.Call =>
        getTypeOfCall(call)
      case ifExpr: AstNode.IfExpr =>
        DataType.getCompatibleType(getType(ifExpr.body), getType(ifExpr.elseBody))
      case lambda: AstNode.Lambda =>
        getTypeOfLambda(lambda)
      case list: AstNode.List =>
        getTypeOfList(list)
      case list: AstNode.ListComp =>
        getTypeOfListComp(list)
      case _: AstNode.Dict =>
        BuiltinTypes.DICT
      case _: AstNode.DictComp =>
        BuiltinTypes.DICT
      case name: AstNode.Name =>
        validateDataType(findName(name.name))
      case subscript: AstNode.Subscript =>
        (getType(subscript.base), subscript.slice) match {
          case (BuiltinTypes.LIST_TYPE, Index(_, value)) =>
            ListType(getType(value))
          case (BuiltinTypes.DICT_TYPE, MultiSlice(_, Array(Index(_, key), Index(_, value)))) =>
            new DictType(getType(key), getType(value))
          case (BuiltinTypes.TUPLE_TYPE, MultiSlice(_, elements)) if elements.forall {
            case Index(_, _) => true
            case _ => false
          } =>
            TupleType(elements.collect { case Index(_, v) => getType(v) })
          case _ => getTypeOfSubscript(subscript)
        }
      case unOp: AstNode.UnaryOp =>
        getType(unOp.expr)
      case value: AstNode.Value =>
        DataType.fromValueType(value.valueType)
      case _ =>
        ANY_TYPE
    }

  protected def getTypeOfAttr(attr: AstNode.Attribute): DataType = {
    getType(attr.base).findField(attr.attr.name) match {
      case Some(result) =>
        validateDataType(result)
      case None =>
        ANY_TYPE
    }
  }

  protected def getTypeOfBinaryOp(binOp: AstNode.BinaryOp): DataType = {
    val left = getType(binOp.left)
    val right = getType(binOp.right)
    binOp.op match {
      case BinOp.CMP_EQ | BinOp.CMP_GEQ | BinOp.CMP_GT | BinOp.CMP_IN |
           BinOp.CMP_IS | BinOp.CMP_IS_NOT | BinOp.CMP_LEQ | BinOp.CMP_LT |
           BinOp.CMP_NEQ | BinOp.CMP_NOT_IN =>
        BuiltinTypes.BOOLEAN
      case BinOp.MUL =>
        if (left.isOf(BuiltinTypes.INTEGER_TYPE))
          right
        else if (right.isOf(BuiltinTypes.INTEGER_TYPE))
          left
        else if (left.isOf(BuiltinTypes.FLOAT_TYPE) && right.isOf(BuiltinTypes.FLOAT_TYPE))
          BuiltinTypes.FLOAT
        else if (left.isOneOf(BuiltinTypes.FLOAT_TYPE, BuiltinTypes.COMPLEX_TYPE) &&
          right.isOneOf(BuiltinTypes.FLOAT_TYPE, BuiltinTypes.COMPLEX_TYPE))
          BuiltinTypes.COMPLEX
        else
          ANY_TYPE
      case BinOp.POW =>
        if (right.isOf(BuiltinTypes.INTEGER_TYPE))
          left
        else
          ANY_TYPE
      case BinOp.IDIV =>
        BuiltinTypes.INTEGER
      case BinOp.MOD =>
        left
      case _ =>
        DataType.getCompatibleType(left, right)
    }
  }

  protected def getTypeOfCall(call: AstNode.Call): DataType =
    getType(call.function) match {
      case function: FunctionType =>
        function.getReturnType match {
          case BuiltinTypes.ECHO_TYPE =>
            if (call.args.length > 0)
              getType(call.args(0))
            else
              ANY_TYPE
          case BuiltinTypes.ECHO2_TYPE =>
            if (call.args.length > 1)
              getType(call.args(1))
            else
              ANY_TYPE
          case BuiltinTypes.ECHO_ITEM_TYPE =>
            if (call.args.length > 0)
              getType(call.args(0)).getItemType
            else
              ANY_TYPE
          case BuiltinTypes.ECHO_RETURN_TYPE =>
            if (call.args.length > 0) {
              val result = getType(call.args(0))
              if (result.isCallable)
                Instance(result.getReturnType)
              else
                ANY_TYPE
            } else
              ANY_TYPE
          case ret =>
            Instance(ret)
        }
      case cls: ClassType =>
        Instance(cls)
      case _ =>
        ANY_TYPE
    }

  protected def getTypeOfLambda(lambda: AstNode.Lambda): DataType =
    if (lambda != null) {
      val params = collection.mutable.ArrayBuffer[Parameter]()
      if (lambda.args != null)
        for (p <- lambda.args.args)
          p match {
            case param: AstNode.NameParameter =>
              params += new Parameter(param.name, ANY_TYPE)
            case _ =>
          }
      new LambdaFunction(params.toArray, getType(lambda.body))
    } else
      BuiltinTypes.ANY_TYPE

  protected def getTypeOfList(list: AstNode.List): DataType =
    if (list.elements.nonEmpty) {
      var result = getType(list.elements.head)
      for (element <- list.elements)
        result = DataType.getCompatibleType(result, getType(element))
      if (result != null && result != ANY_TYPE)
        new Instance(ListType(result))
      else
        BuiltinTypes.LIST
    } else
      BuiltinTypes.LIST

  protected def getTypeOfListComp(listComp: AstNode.ListComp): DataType =
    if (listComp.generators.length == 1)
      listComp.generators.head.target match {
        case AstNode.Name(_, name) =>
          try {
            paramStack.push(TypeAstWalker.Parameter(name, getType(listComp.generators.head.iter).getItemType))
            getType(listComp.elements)
          } finally {
            paramStack.pop()
          }
        case _ =>
          BuiltinTypes.LIST
      }
    else
      BuiltinTypes.LIST

  protected def getTypeOfSubscript(subscript: AstNode.Subscript): DataType =
    subscript.slice match {
      case _: AstNode.Index =>
        getType(subscript.base).getItemType
      case _: AstNode.SliceRange =>
        getType(subscript.base)
      case _ =>
        ANY_TYPE
    }
}
object TypeAstWalker {
  protected case class Parameter(name: String, dataType: DataType)
}
