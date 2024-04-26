/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.lexer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15/05/2016
  * Updated by Tobias Kohn on 26/04/2024
  */
class Scanner(val source: CharSequence) {

  final val catCodes = new CatCodes() {
    update('\\', CatCodes.ESCAPE)
    update('#', CatCodes.COMMENT)
    update('$', CatCodes.INVALID)
    update('?', CatCodes.INVALID)
    update('@', CatCodes.OPERATOR)
    update('>', CatCodes.OPERATOR)
    update('<', CatCodes.OPERATOR)
    update('%', CatCodes.OPERATOR)
    update('&', CatCodes.OPERATOR)
    update('|', CatCodes.OPERATOR)
    update('^', CatCodes.OPERATOR)
    update('=', CatCodes.OPERATOR)
    update('!', CatCodes.OPERATOR)
    update('~', CatCodes.OPERATOR)
    update('`', CatCodes.OPERATOR)
    update(':', CatCodes.DELIMITER)
    update(0x0C.toChar, CatCodes.IGNORE)    // Some std-libraries contain this character
    update(65279.toChar, CatCodes.IGNORE)   // Some std-libraries contain this character
  }

  private var _indentChar: Char = '\u0000'

  private var _pos: Int = 0

  def pos: Int = _pos

  def remaining: Int = source.length - _pos

  def reset(): Unit = {
    _pos = 0
    _indentChar = '\u0000'
  }

  private def _apply(absIndex: Int): Char =
    if (0 <= absIndex && absIndex < source.length)
      source.charAt(absIndex)
    else
      '\u0000'

  def apply(index: Int): Char = _apply(pos + index)

  def asLower(index: Int): Char = _apply(pos + index).toLower

  def testChar(index: Int, chars: Char*): Boolean =
    chars.contains(_apply(pos + index))

  def chatAt(absPos: Int): Char =
    source.charAt(absPos)

  def consume(count: Int): Int = {
    val result = _pos
    _pos = (_pos + count) min (source.length+1)
    result
  }

  def consumeIfMatch(chars: Char*): Boolean =
    if (testChar(0, chars: _*)) {
      _pos += 1
      true
    } else
      false

  def consumeLineBreak(): Boolean = {
    val len =
      if (apply(0) == '\r' && apply(1) == '\n')
        2
      else if (apply(0) == '\n' || apply(0) == '\r')
        1
      else
        0
    _pos += len
    len > 0
  }

  def backupToLastLineBreak(): Unit = {
    var i = _pos-1
    while (i > 0 && catCodes(source.charAt(i)) != CatCodes.NEWLINE)
      i -= 1
    _pos = i
  }

  def isFloatExponent(index: Int): Boolean =
    if (asLower(index) == 'e') {
      val c = apply(index+1)
      ((c == '+' || c == '-') && apply(index+2).isDigit) || c.isDigit
    } else
      false

  private def _isTripleChar(absIndex: Int): Boolean =
    if (0 <= absIndex && absIndex+2 < source.length) {
      val c = source.charAt(absIndex)
      source.charAt(absIndex + 1) == c &&
      source.charAt(absIndex + 2) == c
    } else
      false

  def isTripleChar(index: Int): Boolean =
    _isTripleChar(pos + index)

  def peekString(index: Int, len: Int): String = {
    val start = (pos + index) max 0
    val stop = (pos + index + len) min source.length
    source.subSequence(start, stop).toString
  }

  def getString(absPos: Int, len: Int): String = {
    val start = absPos max 0
    val stop = (absPos + len) min source.length()
    source.subSequence(start, stop).toString
  }

  def getStringLiteral(absPos: Int, len: Int): String = {
    val input = getString(absPos, len)
    if (input == null || input == "")
      return input
    val delimiter = input(0)
    val result = new StringBuilder(input.length, "")
    var i = 0
    while (i < input.length) {
      if (input(i) == '\\') {
        i += 1
        if (i < input.length)
          input(i) match {
            case 'b' => result += '\b'
            case 'f' => result += '\f'
            case 'n' => result += '\n'
            case 'r' => result += '\r'
            case 't' => result += '\t'
            case '\r' if i+1 < input.length && input(i+1) == '\n' => i += 1
            case '\n' | '\r' =>
            case c @ ('\\' | '\'' | '\"') =>
              result += c
            case _ =>
              result += '?'
          }
      } else
      if (input(i) != delimiter)
        result += input(i)
      i += 1
    }
    result.toString
  }

  def getLastNonWhitespaceChar(startPos: Int): Char =
    if (startPos <= source.length) {
      var i = startPos - 1
      while (i >= 0 && !(catCodes(source.charAt(i)) == CatCodes.WHITESPACE))
        i -= 1
      if (i >= 0)
        source.charAt(i)
      else
        '\u0000'
    } else
      '\u0000'

  def getNextNonWhitespaceChar(startPos: Int): Char =
    if (startPos > 0) {
      var i = startPos
      while (i < source.length && !(catCodes(source.charAt(i)) == CatCodes.WHITESPACE))
        i += 1
      if (i < source.length())
        source.charAt(i)
      else
        '\u0000'
    } else
      '\u0000'

  def prefixLength(p: Char=>Boolean, startIndex: Int = 0): Int = {
    val start = (pos + startIndex) max 0
    var i = start
    while (i < source.length && p(source.charAt(i)))
      i += 1
    i - start
  }

  def prefixLength(cc: CatCodes.Value, startIndex: Int): Int = {
    val start = (pos + startIndex) max 0
    var i = start
    while (i < source.length && (catCodes(source.charAt(i)) == cc))
      i += 1
    i - start
  }

  def prefixLength(cc1: CatCodes.Value, cc2: CatCodes.Value, startIndex: Int): Int = {
    @inline
    def isCatCode(cc: CatCodes.Value): Boolean = cc == cc1 || cc == cc2
    val start = (pos + startIndex) max 0
    var i = start
    while (i < source.length && isCatCode(catCodes(source.charAt(i))))
      i += 1
    i - start
  }

  def prefixLength(cc1: CatCodes.Value, cc2: CatCodes.Value, cc3: CatCodes.Value, startIndex: Int): Int = {
    @inline
    def isCatCode(cc: CatCodes.Value): Boolean = cc == cc1 || cc == cc2 || cc == cc3
    val start = (pos + startIndex) max 0
    var i = start
    while (i < source.length && isCatCode(catCodes(source.charAt(i))))
      i += 1
    i - start
  }

  def prefixLengthWithUnderline(p: Char => Boolean, startIndex: Int = 0): Int = {
    val start = (pos + startIndex) max 0
    var i = start
    while (i < source.length && p(source.charAt(i))) {
      while (i < source.length && p(source.charAt(i))) {
        i += 1
      }
      if (i + 1 < source.length && source.charAt(i) == '_' && p(source.charAt(i + 1)))
        i += 1
    }
    i - start
  }

  def suffixLength(startIndex: Int, ccs: CatCodes.Value*): Int = {
    @inline
    def isCatCode(cc: CatCodes.Value): Boolean = ccs.contains(cc)
    val start = (pos + startIndex) max 0
    var i = start-1
    while (i > 0 && isCatCode(catCodes(source.charAt(i))))
      i -= 1
    (start-1) - i
  }

  private[lexer] def getIndentationCharIfConsistent(len: Int): Char =
    if (len > 0) {
      if (_indentChar == '\u0000')
        _indentChar = source.charAt(pos)
      val ch = _indentChar
      for (i <- 0 until len)
        if (source.charAt(pos + i) != ch)
          return '\u0000'
      ch
    } else
      ' '

  def skip(p: Char=>Boolean): Unit =
    consume(prefixLength(p))

  def skipLine(): Unit = {
    val len = prefixLength(c => catCodes(c) != CatCodes.NEWLINE)
    consume(len)
  }

  def isFollowingIndentation: Boolean = {
    var i = pos - 1
    while (i >= 0 && !catCodes.isNewline(source.charAt(i)))
      if (catCodes.isWhitespace(source.charAt(i)))
        i -= 1
      else
        return false
    true
  }

  def peekNonWhiteChar(): Char = {
    var i = pos
    while (i < source.length && catCodes.isWhitespace(source.charAt(i)))
      i += 1
    if (i < source.length)
      source.charAt(i)
    else
      '\u0000'
  }

  def lineFromPosition(position: Int): Int =
    if (0 <= position && position <= source.length()) {
      var result: Int = 0
      var i = 0
      while (i < position) {
        if (source.charAt(i) == '\n')
          result += 1
        i += 1
      }
      result
    } else
      0

  def lineOffsetFromPosition(position: Int): Int =
    if (0 <= position && position <= source.length()) {
      @inline
      def isNewline(c: Char): Boolean = c == '\n' || c == '\r'
      var i = position-1
      while (i >= 0 && !isNewline(source.charAt(i)))
        i -= 1
      position - (i+1)
    } else
      0

  def isCompoundStatement(position: Int): Boolean =
    if (0 <= position) {
      var i = position
      while (i < source.length && source.charAt(i).isLower)
        i += 1
      if (i < source.length && !source.charAt(i).isLetterOrDigit && source.charAt(i) != '_') {
        val word = source.subSequence(position, i).toString
        Set("def", "if", "while", "for", "with", "class").contains(word)
      } else
        false
    } else
      false
}