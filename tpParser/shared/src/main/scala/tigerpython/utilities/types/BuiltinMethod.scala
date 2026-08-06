package tigerpython.utilities.types

/**
 * The `BuiltinMethod` represents special methods such as `list.pop()` where we need to access the type information
 * of the underlying receiver in order to get accurate types.
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

  // `parent` is bound once, when the field is registered on its declaring type (e.g. `<mutable-seq>`),
  // so it never reflects the receiver's own type (e.g. `list[Actor]` rather than plain `list`). Every
  // attribute access must therefore call `boundTo` with the actual receiver type so `getMethodType` sees it.
  def boundTo(receiver: PrimitiveType): BuiltinMethod = {
    val result = new BuiltinMethod(receiver, name, params, defaultReturnType)
    result.docString = docString
    result
  }

  override def toString: String = "%s(%s)".format(name, getParamsString)
}
object BuiltinMethod {

  def apply(parent: PrimitiveType, name: String, params: Array[String], docString: String,
            defaultReturnType: DataType = BuiltinTypes.ANY_TYPE): BuiltinMethod = {
    val result = new BuiltinMethod(parent, name, params, defaultReturnType)
    result.docString = docString
    result
  }
}
