package tigerpython.utilities
package completer

import types.DataType

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15.06.2016.
  * Updated by Tobias Kohn on 20.06.2016.
  */
class ImportFromFilter(val startPos: Int, val base: DataType) extends DefaultNameFilter {
  for ((name, field) <- base.getFields)
    addName(name, field)
  override def getParams(name: String): String = null
}
