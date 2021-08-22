package tigerpython.utilities
package completer

import scopes.ModuleLoader

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15.06.2016.
  * Updated by Tobias Kohn on 20.06.2016.
  */
class ImportFilter(val moduleLoader: ModuleLoader) extends DefaultNameFilter {

  if (moduleLoader != null)
    for ((name, module) <- moduleLoader.getModulesList)
      addName(name, module)
  else
    for ((name, module) <- ModuleLoader.modules)
      addName(name, module)

  override def getParams(name: String): String = null
}
