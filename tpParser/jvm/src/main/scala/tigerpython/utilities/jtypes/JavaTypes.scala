package tigerpython.utilities
package jtypes

import types.{BuiltinTypes, DataType}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 27.06.2016.
  */
object JavaTypes {

  private val JavaBool = classOf[java.lang.Boolean]
  private val JavaChar = classOf[java.lang.Character]
  private val JavaDouble = classOf[java.lang.Double]
  private val JavaFloat = classOf[java.lang.Float]
  private val JavaInt = classOf[java.lang.Integer]
  private val JavaLong = classOf[java.lang.Long]
  private val JavaString = classOf[java.lang.String]
  private val JavaVoid = classOf[java.lang.Void]
  private val ScalaArray = classOf[Array[_]]
  private val ScalaBool = classOf[Boolean]
  private val ScalaChar = classOf[Char]
  private val ScalaDouble = classOf[Double]
  private val ScalaFloat = classOf[Float]
  private val ScalaInt = classOf[Int]
  private val ScalaString = classOf[String]
  private val ScalaUnit = classOf[Unit]

  def fromJavaType(javaType: Class[_]): DataType =
    javaType match {
      case JavaBool | ScalaBool =>
        BuiltinTypes.BOOLEAN_TYPE
      case JavaInt | JavaLong | ScalaInt =>
        BuiltinTypes.INTEGER_TYPE
      case JavaString | ScalaString | JavaChar | ScalaChar =>
        BuiltinTypes.STRING_TYPE
      case JavaDouble | JavaFloat | ScalaDouble | ScalaFloat =>
        BuiltinTypes.FLOAT_TYPE
      case JavaVoid | ScalaUnit =>
        BuiltinTypes.NONE_TYPE
      case ScalaArray =>
        BuiltinTypes.LIST_TYPE
      case _ =>
        val result = JavaClass(javaType)
        if (result == null) {
          if (javaType.isArray)
            BuiltinTypes.LIST_TYPE
          else
            BuiltinTypes.ANY_TYPE
        } else
          result
    }
}
