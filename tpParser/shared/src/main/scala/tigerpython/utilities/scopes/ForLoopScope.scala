package tigerpython.utilities
package scopes

import types.DataType

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 01.07.2016.
  */
class ForLoopScope(val startPos: Int, val endPos: Int, val loopVars: Map[String, DataType]) extends Scope {

  def addGlobal(name: String): Unit =
    parent match {
      case forLoop: ForLoopScope =>
        forLoop.addGlobal(name)
      case function: FunctionScope =>
        function.addGlobal(name)
      case _ =>
    }

  def define(name: String, dataType: DataType): Unit =
    if (!loopVars.contains(name))
      parent.define(name, dataType)

  def isLocal(name: String): Boolean = parent.isLocal(name)

  def getLocals: Map[String, DataType] =
    parent.getLocals ++ loopVars
}
