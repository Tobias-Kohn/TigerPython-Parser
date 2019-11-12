package tigerpython.parser

import scala.scalajs.js.annotation._

/**
  * A record-structure that holds the information pertaining a single error.
  *
  * @param line    The line number where the error occurs, starting from 1.
  * @param offset  The offset within the line where the error occurs, or possibly 0 if there is no clear offset.
  * @param msg     The translated message in the user's language, ready for display.
  * @param code    The message code (as a string), which is usually rather of interest for internal purposes such as
  *                gathering statistics.  There is no sense in displaying this to the user.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 10/08/2016
  * Updated by Tobias Kohn on 12/11/2019
  */
@JSExportAll
case class ErrorInfo(line: Int, offset: Int, msg: String, code: String)