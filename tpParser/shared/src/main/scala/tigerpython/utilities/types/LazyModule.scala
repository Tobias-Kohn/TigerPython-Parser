package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 20.06.2016.
  * Updated by Tobias Kohn on 28.06.2016.
  */
class LazyModule(val name: String, private val subModules: Map[String, Package]) extends Package {

  protected lazy val fields = {
    val result = new NameMap()
    for ((name, module) <- subModules)
      result(name) = module
    result
  }
  var source: String = name

  override def getFullName: String = source

  def getFields: Map[String, DataType] = fields.toMap

  def setField(name: String, dataType: DataType): Unit =
    fields(name) = dataType
}
