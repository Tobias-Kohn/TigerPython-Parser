package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 14.06.2016.
  */
abstract class FunctionType extends DataType {
  def getFields: Map[String, DataType] = Map()
  override def isCallable: Boolean = true
  def setField(name: String, dataType: DataType): Unit = {}
}
