package tigerpython.utilities
package completer

import scopes.{BuiltinNames, Scope}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 10.06.2016.
  * Updated by Tobias Kohn on 19.06.2016.
  */
class ScopeFilter(val scope: Scope) extends DefaultNameFilter {

  for ((key, value) <- BuiltinNames.builtins)
    addName(key, value)
  if (scope != null)
    for ((name, dataType) <- scope.getAllLocals) {
      addName(name, dataType)
      dataType match {
        case self: types.SelfInstance =>
          for ((field, dt) <- self.getFields)
            addName("%s.%s".format(name, field), dt)
        case _ =>
      }
    }
}
