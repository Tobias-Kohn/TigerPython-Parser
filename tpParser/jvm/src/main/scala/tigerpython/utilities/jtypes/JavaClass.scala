package tigerpython.utilities.jtypes

import java.lang.reflect.Modifier

import tigerpython.utilities.types.{ClassType, DataType, NameMap}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 27.06.2016.
  */
class JavaClass(val source: Class[_]) extends ClassType {

  lazy val baseType = JavaClass(source.getSuperclass)

  val name = if (source != null) source.getSimpleName else "?"
  override def getFullName: String = if (source != null) source.getName else "?"

  private def stripGet(name: String): String =
    name(3).toLower + name.drop(4)

  protected lazy val (instanceFields, classFields, protectedFields) = {
    def isPublic(mod: Int): Boolean =
      Modifier.isPublic(mod) && !Modifier.isAbstract(mod)
    val instFields = new NameMap()
    val statFields = new NameMap()
    val protFields = new NameMap()
    for (method <- source.getMethods)
      if (isPublic(method.getModifiers)) {
        val name = method.getName
        val javaMethod = JavaMethod(this, method)
        if (Modifier.isStatic(method.getModifiers))
          statFields(name) = javaMethod
        else {
          instFields(name) = javaMethod
          if (name.length > 3 && name.startsWith("get") && name(3).isUpper &&
              method.getParameterCount == 0 && !name.drop(3).forall(_.isUpper))
            instFields(stripGet(name)) = javaMethod.returnType
        }
      } else
      if (Modifier.isProtected(method.getModifiers) && !Modifier.isStatic(method.getModifiers)) {
        val name = method.getName
        val javaMethod = JavaMethod(this, method)
        protFields(name) = javaMethod
        if (name.length > 3 && name.startsWith("get") && name(3).isUpper &&
            method.getParameterCount == 0 && !name.drop(3).forall(_.isUpper))
          protFields(stripGet(name)) = javaMethod.returnType
      }
    for (field <- source.getFields
         if Modifier.isPublic(field.getModifiers)) {
      val name = field.getName
      val dataType =
        if (field.getType == source)
          this
        else
          JavaTypes.fromJavaType(field.getType)
      if (Modifier.isStatic(field.getModifiers))
        statFields(name) = dataType
      else
        instFields(name) = dataType
    }
    (instFields.toMap, statFields.toMap, protFields.toMap)
  }

  def getFields: Map[String, DataType] = classFields

  def getInstanceFields: Map[String, DataType] = instanceFields

  override def getParamsString: String = ""

  override def getProtectedFields: Map[String, DataType] = protectedFields

  def isSubclassOf(base: DataType): Boolean =
    if (this == base)
      true
    else if (baseType != null)
      baseType.isSubclassOf(base)
    else
      false

  def setField(name: String, dataType: DataType): Unit = {}
}
object JavaClass {

  private val javaClasses = collection.mutable.Map[Class[_], JavaClass]()

  val JAVA_OBJECT = new JavaClass(classOf[java.lang.Object])
  javaClasses(JAVA_OBJECT.source) = JAVA_OBJECT

  apply(classOf[java.awt.Color])
  apply(classOf[java.awt.Dimension])
  apply(classOf[java.awt.Font])
  apply(classOf[java.awt.Point])

  def apply(cls: Class[_]): JavaClass =
    if (cls != null)
      javaClasses.getOrElseUpdate(cls, new JavaClass(cls))
    else
      null
}
