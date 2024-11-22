/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.errors

import java.util.MissingFormatArgumentException

import tigerpython.parser.errormessages

/**
  * This class is responsible for creating human-readable error-messages. By default is just takes the string-values
  * associated with each error message and interweaves the parameters.
  *
  * Write a translator of your own to provide customized error messages, possibly in another language. Then set the
  * `translator`-field of your `ErrorHandler` to your new translator-object.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 28/05/2016
  * Updated by Tobias Kohn on 12/11/2019
  */
trait ErrorTranslator {
  var language: String

  val languages:Array[String] = Array("en", "de", "nl", "fr", "it", "ru")

  def messageToString(msg: ErrorCode.Value, params: Seq[AnyRef]): String

  def setMessage(code: ErrorCode.Value, msg: String): Boolean = false
}
object ErrorTranslator {

  /**
    * Default error translator.
    */
  object DefaultErrorTranslator extends ErrorTranslator {
    private var _lang: String = "en"
    def language: String = _lang
    def language_=(l: String): Unit =
      l.toLowerCase match {
        case lang if languages.contains(lang) =>
          _lang = lang
        case _ =>
      }

    def messageToString(msg: ErrorCode.Value, params: Seq[AnyRef]): String = {
      var baseMessage =
        _lang match {
          case "de" =>
            errormessages.GermanMessages.getMessage(msg)
          case "nl" =>
            errormessages.DutchMessages.getMessage(msg)
          case "fr" =>
            errormessages.FrenchMessages.getMessage(msg)
          case "it" =>
            errormessages.ItalianMessages.getMessage(msg)
          case "ru" =>
            errormessages.RussianMessages.getMessage(msg)
          case _ =>
            errormessages.EnglishMessages.getMessage(msg)
        }
      if (baseMessage == null)
        baseMessage = errormessages.EnglishMessages.getMessage(msg)
      try {
        baseMessage.format(params.map(_.toString): _*)
      } catch {
        case _: MissingFormatArgumentException =>
          baseMessage
      }
    }

    override def setMessage(code: ErrorCode.Value, msg: String): Boolean =
      _lang match {
        case "de" =>
          errormessages.GermanMessages.setMessage(code, msg)
          true
        case "en" =>
          errormessages.EnglishMessages.setMessage(code, msg)
          true
        case "fr" =>
          errormessages.FrenchMessages.setMessage(code, msg)
          true
        case "nl" =>
          errormessages.DutchMessages.setMessage(code, msg)
          true
        case "it" =>
          errormessages.ItalianMessages.setMessage(code, msg)
          true
        case "ru" =>
          errormessages.RussianMessages.setMessage(code, msg)
          true
        case _ =>
          false
      }
  }

  var errorTranslator: ErrorTranslator = DefaultErrorTranslator
}
