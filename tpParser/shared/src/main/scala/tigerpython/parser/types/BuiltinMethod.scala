package tigerpython.parser.types

/**
 * The `BuiltinMethod` represents special methods such as `dict.getKeys()` where we need to access the type information
 * of the underlying object in order to get accurate types.
 */
class BuiltinMethod(var parent: PrimitiveType,
                    val name: String,
                    val params: Array[String],
                    val defaultReturnType: DataType) extends FunctionType {

  override def getFullName: String = if (parent != null) parent.getFullName + "." + name else name

  override def getParamsString: String = params.mkString(", ")

  override def getReturnType: DataType =
    if (parent != null)
      parent.getMethodType(name)
    else
      defaultReturnType

  override def toString: String = "%s(%s)".format(name, getParamsString)
}
object BuiltinMethod {

  def apply(parent: PrimitiveType, name: String, params: Array[String], docString: String,
            defaultReturnType: DataType = BuiltinTypes.ANY_TYPE): BuiltinMethod = {
    val result = new BuiltinMethod(parent, name, params, defaultReturnType)
    result.docString = docString
    result
  }

  def fromString(parent: PrimitiveType, s: String): BuiltinMethod =
    if (s != null && s.nonEmpty) {
      val (retType, source) =
        if (s(0) == '[')
          (BuiltinTypes.fromString(s.drop(1).takeWhile(_ != ']')), s.dropWhile(_ != ']').drop(1))
        else
          (BuiltinTypes.ANY_TYPE, s)
      val name = source.takeWhile(_ != '(')
      val params = source.drop(name.length+1).takeWhile(_ != ')').filter(_ != ' ').split(',')
      val docString = source.dropWhile(_ != ')').dropWhile(!_.isLetterOrDigit)
      apply(parent, name, params, docString, retType)
    } else
      null
}