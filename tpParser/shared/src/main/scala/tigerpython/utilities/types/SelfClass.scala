package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 28.06.2016.
  * Updated by Tobias Kohn on 28.06.2016.
  */
class SelfClass(baseType: PythonClass) extends DataType  {

  def name: String =
    if (baseType != null)
      "SelfClass[%s]".format(baseType.name)
    else
      "SelfClass"

  def getFields: Map[String, DataType] =
    if (baseType != null)
      baseType.getFields
    else
      Map()

  def setField(name: String, dataType: DataType): Unit =
    if (baseType != null)
      baseType.setField(name, dataType)
}
