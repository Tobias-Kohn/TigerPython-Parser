package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 15.09.2016.
  */
class PythonFunction(val name: String,
                     val params: Array[Parameter],
                     val paramCount: Int,
                     var returnType: DataType) extends FunctionType {
  var source: String = _
  var sourcePos: Int = -1

  protected val fields = new NameMap()

  override def getFields: Map[String, DataType] = fields.toMap

  override def getFullName: String =
    if (source != null)
      source
    else
      super.getFullName

  override def getParamsString: String =
    if (isMethod && params.nonEmpty)
      params.drop(1).map(_.name).take(paramCount).mkString(", ")
    else
      params.map(_.name).take(paramCount).mkString(", ")

  override def getReturnType: DataType = returnType

  lazy val isMethod: Boolean =
    if (params.nonEmpty)
      params(0).dataType match {
        case _: SelfInstance | _: SelfClass => true
        case _ => false
      }
    else
      false

  override def setField(name: String, dataType: DataType): Unit =
    fields(name) = dataType
}
object PythonFunction {
  // Note: the methods are assumed to have self as first param if they are a method of a class
  def fromString(s: String, methodOfClass: PythonClass, localTypes: Map[String, DataType]): PythonFunction =
    if (s != null && s.length > 0) {
      val (retType, source) =
        if (s(0) == '[') {
          val name = s.drop(1).takeWhile(_ != ']')
          (if (localTypes.contains(name)) localTypes(name) else BuiltinTypes.fromString(name), s.dropWhile(_ != ']').drop(1))
        } else
          (BuiltinTypes.ANY_TYPE, s)
      val name = source.takeWhile(_ != '(')
      val rawParams = source.drop(name.length+1).takeWhile(_ != ')').filter(_ != ' ').split(',')
      if (rawParams(0) != "self")
        null
      else {
        val params = Parameter("self", new SelfInstance(methodOfClass)) +:
          rawParams.drop(1).map(p => Parameter(p, BuiltinTypes.ANY_TYPE))
        new PythonFunction(name, params, params.length, retType)
      }
    } else
      null
}
