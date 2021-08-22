package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 27.06.2016.
  */
abstract class ClassType extends DataType {
  def getInstanceFields: Map[String, DataType]
  def getProtectedFields: Map[String, DataType] = Map()
  override def isCallable: Boolean = true
  def isSubclassOf(base: DataType): Boolean
}
