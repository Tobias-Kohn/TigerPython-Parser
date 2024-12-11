package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 11.12.2024.
  */
class BuiltinFunction(val name: String,
                      val params: Array[String],
                      val returnType: DataType) extends FunctionType {
  var parent: DataType = _

  override def getFullName: String = if (parent != null) parent.getFullName + "." + name else name

  override def getParams: Array[String] = params
  override def getParamsString: String = params.mkString(", ")
  override def getReturnType: DataType = returnType

  override def toString: String = "%s(%s)".format(name, getParamsString)
}
object BuiltinFunction {

  def apply(name: String, params: Array[String], returnType: DataType, docString: String): BuiltinFunction = {
    val result = new BuiltinFunction(name, params, returnType)
    result.docString = docString
    result
  }

  def fromString(s: String): BuiltinFunction =
    if (s != null && s.length > 0) {
      val (retType, source) =
        if (s(0) == '[')
          (BuiltinTypes.fromString(s.drop(1).takeWhile(_ != ']')), s.dropWhile(_ != ']').drop(1))
        else
          (BuiltinTypes.ANY_TYPE, s)
      val name = source.takeWhile(_ != '(')
      val params = source.drop(name.length+1).takeWhile(_ != ')').filter(_ != ' ').split(',')
      val docString = source.dropWhile(_ != ')').dropWhile(!_.isLetterOrDigit)
      apply(name, params, retType, docString)
    } else
      null
}
