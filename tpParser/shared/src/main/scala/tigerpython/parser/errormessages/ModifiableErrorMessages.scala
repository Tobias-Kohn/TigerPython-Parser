package tigerpython.parser
package errormessages

import errors._

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 12/11/2019
  * Updated by Tobias Kohn on 12/11/2019
  */
abstract class ModifiableErrorMessages {

  private val messages = collection.mutable.Map[ErrorCode.Value, String]()

  protected def _getMessage(code: ErrorCode.Value): String

  def getMessage(code: ErrorCode.Value): String =
    messages.get(code) match {
      case Some(msg) =>
        msg
      case None =>
        _getMessage(code)
    }

  def reset(): Unit =
    messages.clear()

  def setMessage(code: ErrorCode.Value, msg: String): Unit =
    if (msg == null || msg == "")
      messages.remove(code)
    else
      messages(code) = msg
}
