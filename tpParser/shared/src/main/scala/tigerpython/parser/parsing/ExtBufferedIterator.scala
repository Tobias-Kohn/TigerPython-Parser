/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 03/06/2016
  * Updated by Tobias Kohn on 06/07/2016
  */
object ExtBufferedIterator {

  def apply[T >: Null](source: Iterator[T]): ExtBufferedIterator[T] =
    new CachedIterator[T](source)

  def apply[T >: Null](source: Seq[T]): ExtBufferedIterator[T] =
    new SequenceIterator[T](source)

  class CachedIterator[T >: Null](val source: Iterator[T]) extends ExtBufferedIterator[T] {

    private val cache = collection.mutable.Queue[T]()

    def hasNext: Boolean = cache.nonEmpty || source.hasNext

    def head: T = {
      if (cache.isEmpty && source.hasNext)
        cache += source.next()
      if (cache.nonEmpty)
        cache.head
      else
        null
    }

    def next(): T =
      if (cache.nonEmpty)
        cache.dequeue()
      else if (source.hasNext)
        source.next()
      else
        null

    def peek(index: Int): T =
      if (index > 0) {
        while (cache.length <= index && source.hasNext)
          cache += source.next()
        if (index < cache.length)
          cache(index)
        else
          null
      } else
      if (index == 0)
        head
      else
        null
  }

  class SequenceIterator[T >: Null](val source: Seq[T]) extends ExtBufferedIterator[T] {

    private var _index: Int = 0

    def hasNext: Boolean = _index < source.length

    def head: T =
      if (_index < source.length)
        source(_index)
      else
        null

    def next(): T =
      if (_index < source.length) {
        val result = source(_index)
        _index += 1
        result
      } else
        null

    def peek(index: Int): T = {
      val idx = _index + index
      if (0 <= idx && idx < source.length)
        source(idx)
      else
        null
    }
  }
}
trait ExtBufferedIterator[T >: Null] extends BufferedIterator[T] {
  def peek(index: Int): T
}
