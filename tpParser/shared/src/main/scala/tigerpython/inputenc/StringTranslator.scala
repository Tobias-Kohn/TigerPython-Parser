package tigerpython.inputenc

/**
  * On some systems it can be hard to input the proper ASCII characters required for Python code.  On tablet computers,
  * for instance, we noticed a tendency to insert typographic quotation marks instead of the straight ones.  This has
  * the unfortunate effect that it is not possible to enter string values.
  *
  * This object provides the means to map special input characters to their ASCII counterparts.  Care is taken that this
  * mapping does not take place inside string literals, say.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 19/11/2019
  * Updated by Tobias Kohn on 18/12/2019
  */
class StringTranslator(val source: String) extends Iterator[Char] {

  import StringTranslator.StringMode

  private var index: Int = 0

  private var stringMode: StringMode.Value = StringMode.NONE

  override def hasNext: Boolean = index < source.length

  override def next(): Char =
    if (index < source.length) {
      val ch = source(index)
      if ((ch == '\"' || ch == '\'') && (prev != '\\'))
        updateStringMode(ch)
      index += 1
      if (stringMode == StringMode.NONE)
        StringTranslator.charMap.getOrElse(ch, ch)
      else
        ch
    } else
      '\u0000'

  private def prev: Char =
    if (index > 0)
      source(index-1)
    else
      '\u0000'

  private def isTriple: Boolean =
    if (index > 2) {
      val ch = source(index)
      source(index-2) == ch && source(index-1) == ch
    } else
      false

  private def updateStringMode(ch: Char): Unit =
    if (ch == '\'')
      stringMode match {
        case StringMode.NONE if isTriple =>
          stringMode = StringMode.SINGLE_TRIPLE
        case StringMode.NONE =>
          stringMode = StringMode.SINGLE
        case StringMode.SINGLE =>
          stringMode = StringMode.NONE
        case StringMode.SINGLE_TRIPLE if isTriple =>
          stringMode = StringMode.NONE
        case _ =>
      }
    else
      stringMode match {
        case StringMode.NONE if isTriple =>
          stringMode = StringMode.DOUBLE_TRIPLE
        case StringMode.NONE =>
          stringMode = StringMode.DOUBLE
        case StringMode.DOUBLE =>
          stringMode = StringMode.NONE
        case StringMode.DOUBLE_TRIPLE if isTriple =>
          stringMode = StringMode.NONE
        case _ =>
      }
}
object StringTranslator {

  private object StringMode extends Enumeration {
    final val NONE = Value
    final val SINGLE = Value
    final val DOUBLE = Value
    final val SINGLE_TRIPLE = Value
    final val DOUBLE_TRIPLE = Value
  }

  /**
    * This maps various "typographic" quotations marks and delimiters to the standard ASCII versions.
    *
    * We do not translate all character variants (such as numbers, letters), but only punctuation marks.  The rationale
    * behind this is that these stand on their own and are sometimes translated incorrectly by the input device.
    * Letters and numbers, on the other hand, might be part of names that are supposed to be distinct (which is
    * probably a bad idea in the first place).
    */
  private lazy val charMap: Map[Char, Char] = Map(
    '«' -> '"',
    '»' -> '"',
    '\u2018' -> '\'',
    '\u2019' -> '\'',
    '\u201A' -> '\'',
    '\u201C' -> '\"',
    '\u201D' -> '\"',
    '\u201E' -> '\"',
    '\u3001' -> ',',
    '\u3002' -> '.',
    '\u301E' -> '\"',
    '\uFF02' -> '\"',
    '\uFF03' -> '#',
    '\uFF05' -> '%',
    '\uFF06' -> '&',
    '\uFF07' -> '\'',
    '\uFF08' -> '(',
    '\uFF09' -> ')',
    '\uFF0A' -> '*',
    '\uFF0B' -> '+',
    '\uFF0C' -> ',',
    '\uFF0D' -> '-',
    '\uFF0E' -> '.',
    '\uFF0F' -> '/',
    '\uFF1A' -> ':',
    '\uFF1B' -> ';',
    '\uFF1C' -> '<',
    '\uFF1D' -> '=',
    '\uFF1E' -> '>',
    '\uFF3B' -> '[',
    '\uFF3D' -> ']',
    '\uFF5B' -> '{',
    '\uFF5D' -> '}',
    '\uFF61' -> '.'
  )

  def translate(ch: Char): Char =
    charMap.getOrElse(ch, ch)

  /**
    * Returns a translation of the source string.  The result is a string of exactly the same length as the original,
    * however with some of the characters possibly changed.
    */
  def translate(s: String): String =
    if (s != null && s != "") {
      val result = new Array[Char](s.length)
      val translator = new StringTranslator(s)
      for (i <- result.indices)
        result(i) = translator.next()
      new String(result)
    } else
      s
}