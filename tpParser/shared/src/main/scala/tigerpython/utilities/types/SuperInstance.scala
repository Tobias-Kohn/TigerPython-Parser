package tigerpython.utilities.types

/**
  * The result of a `super()` call: a view of the parent classes (in the order given) as seen from
  * a method of the class that has them as `bases`.  Members of earlier bases take precedence.
  */
class SuperInstance(bases: Array[ClassType]) extends DataType {

  def name: String = "super[%s]".format(bases.map(_.name).mkString(", "))

  def getFields: Map[String, DataType] =
    bases.foldRight(Map[String, DataType]())((base, result) =>
      result ++ base.getInstanceFields ++ base.getProtectedFields)

  def setField(name: String, dataType: DataType): Unit = {}
}
