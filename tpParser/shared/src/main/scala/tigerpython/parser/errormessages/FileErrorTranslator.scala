/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package errormessages

import errors.{ErrorCode, ErrorTranslator}

/**
  * Reads in a map of error messages from a file/external source.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 30/06/2016
  * Updated by Tobias Kohn on 09/11/2019
  */
class FileErrorTranslator extends ErrorTranslator {

  var language: String = _

  protected val messages: collection.mutable.Map[ErrorCode.Value, String] =
    collection.mutable.Map[ErrorCode.Value, String]()

  protected def getBaseMessage(msg: ErrorCode.Value): String =
    messages.getOrElse(msg, null)

  def loadFromString(source: String): Unit = {
    val lines = source.split("\n")

    var msg: ErrorCode.Value = ErrorCode.UNKNOWN
    val text = collection.mutable.ArrayBuffer[String]()

    def flush(): Unit = {
      if (msg != ErrorCode.UNKNOWN && text.nonEmpty)
        messages(msg) = text.mkString("\n")
      msg = ErrorCode.UNKNOWN
      text.clear()
    }

    for (line <- lines
         if line != "")
      if (line.startsWith("case ")) {
        flush()
        var msgText = line.drop(4).dropWhile(_ == ' ').takeWhile(x => x != ':' && x != '=')
        while (msgText.length > 1 && msgText.last == ' ')
          msgText = msgText.dropRight(1)
        msgText = msgText.toUpperCase.replace(' ', '_').replace('-', '_')
        try {
          msg = ErrorCode.withName(msgText)
        } catch {
          case _: Throwable => msg = ErrorCode.UNKNOWN
        }
        val idx = line.indexOf("=>")
        if (idx > 0)
          text += line.drop(idx+2).dropWhile(_ == ' ')
      } else
      if (!line.startsWith("//") && !line.startsWith("#")){
        if (line.startsWith("|"))
          text += line.drop(1)
        else
          text += line.dropWhile(_ == ' ')
      }
    flush()
  }

  def messageToString(msg: ErrorCode.Value, params: Seq[AnyRef]): String = {
    val message = getBaseMessage(msg)
    if (message != null)
      message.format(params.map(_.toString): _*)
    else
      ErrorTranslator.errorTranslator.messageToString(msg, params)
  }
}
