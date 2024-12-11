package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 11.12.2024.
  */
abstract class Package extends DataType {
  protected[types] var parent: Package = _
  override def getTypeName: String = "module"
}
