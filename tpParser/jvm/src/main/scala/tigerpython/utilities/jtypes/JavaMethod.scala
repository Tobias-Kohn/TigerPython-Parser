package tigerpython.utilities.jtypes

import java.lang.reflect.Method

import tigerpython.utilities.types.{BuiltinTypes, DataType, FunctionType}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 29.03.2017.
  */
class JavaMethod(val name: String, val params: Array[String], val returnType: DataType) extends FunctionType {
  override def getParamsString: String = params.mkString(", ")
  override def getReturnType: DataType = returnType
}
object JavaMethod {

  def apply(cls: JavaClass, method: Method): JavaMethod =
    try {
      val name = method.getName
      val params = collection.mutable.ArrayBuffer[String]()
      val paramCount = method.getParameterCount
      if (paramCount > 0) {
        for (i <- 0 until paramCount) {
          val p = method.getParameters()(i)
          params += p.getName
        }
      }
      val retType = if (method.getReturnType == cls.source)
          cls
        else
          JavaTypes.fromJavaType(method.getReturnType)
      new JavaMethod(name, params.toArray, retType)
    } catch {
      case _: NoSuchMethodError | _: NoSuchMethodException =>
        new JavaMethod(method.getName, Array(), BuiltinTypes.ANY_TYPE)
    }
}
