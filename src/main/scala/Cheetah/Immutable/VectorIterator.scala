package Cheetah.Immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.{specialized => sp}

class VectorIterator[@sp +A](startIndex: Int,
                          override private[Immutable] val endIndex: Int)
  extends Iterator[A]
    with VectorPointer[A @uncheckedVariance] {

  /*Index in the vector of the first element of the current block, i.e. current display0 */
  private var blockIndex: Int = _
  /*Index in the current block, i.e. current display0 */
  private var lo: Int = _
  /*End index (or length) of the current block, i.e. current display0 */
  private var endLo: Int = _
  private var _hasNext: Boolean = _

  final private[Immutable] def initIteratorFrom[@sp B >: A](
                                                             that: VectorPointer[B]): Unit = {
    initWithFocusFrom(that)
    _hasNext = startIndex < endIndex
    if (_hasNext) {
      focusOn(startIndex)
      blockIndex = focusStart + (focus & -32)
      lo = focus & 31
      if (endIndex < focusEnd)
        focusEnd = endIndex
      endLo = spire.math.min(focusEnd - blockIndex, 32)
    } else {
      blockIndex = 0
      lo = 0
      endLo = 1
      display0 = new Leaf(1)
    }
  }

  final def hasNext: Boolean = _hasNext

  def next(): A = {
    var _lo = lo
    val res: A = display0(_lo)
    _lo += 1
    lo = _lo
    val _endLo = endLo
    if (_lo == _endLo)
      gotoNextBlock()
    res
  }

  final private[Immutable] def gotoNextBlock(): Unit = {

    val oldBlockIndex = blockIndex
    val newBlockIndex = oldBlockIndex + endLo

    blockIndex = newBlockIndex
    lo = 0

    if (newBlockIndex < focusEnd) {
      val _focusStart = focusStart
      val newBlockIndexInFocus = newBlockIndex - _focusStart
      gotoNextBlockStart(newBlockIndexInFocus,
        newBlockIndexInFocus ^ (oldBlockIndex - _focusStart)
      )
      endLo = spire.math.min(focusEnd - newBlockIndex, 32)
    } else if (newBlockIndex < endIndex) {
      focusOn(newBlockIndex)
      if (endIndex < focusEnd)
        focusEnd = endIndex
      endLo = spire.math.min(focusEnd - newBlockIndex, 32)
    } else {
      lo = 0
      blockIndex = endIndex
      endLo = 1
      if (_hasNext) {
        _hasNext = false
      } else
        throw new NoSuchElementException("reached iterator end")
    }
  }

  private[Immutable] def remaining: Int =
    spire.math.max(endIndex - blockIndex + lo, 0)
}
