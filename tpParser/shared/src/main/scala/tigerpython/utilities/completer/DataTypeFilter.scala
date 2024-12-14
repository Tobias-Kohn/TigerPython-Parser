package tigerpython.utilities
package completer

import types.{DataType, SelfInstance}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 10.06.2016.
  * Updated by Tobias Kohn on 14.12.2024.
  */
class DataTypeFilter(val startPos: Int, val base: DataType) extends DefaultNameFilter {

  hideProtected = !base.isInstanceOf[SelfInstance]

  for ((name, field) <- base.getFields)
    addName(name, field)
}
