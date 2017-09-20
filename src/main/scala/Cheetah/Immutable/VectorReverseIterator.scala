package Cheetah.Immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.{specialized => sp}

class VectorReverseIterator[@sp +A](startIndex: Int,
                                    final override private[Immutable] val endIndex: Int)
  extends Iterator[A]
    with VectorPointer[A @uncheckedVariance] {

  private var lastIndexOfBlock: Int = _
  /*Index in the current block, i.e. current display0 */
  private var lo: Int = _
  /*End index (or length) of the current block, i.e. current display0 */
  private var endLo: Int = _
  private var _hasNext: Boolean = startIndex < endIndex

  final private[Immutable] def initIteratorFrom[@sp B >: A](that: VectorPointer[B]): Unit = {
    initWithFocusFrom(that.asInstanceOf[VectorPointer[A]])
    _hasNext = startIndex < endIndex
    if (_hasNext) {
      val idx = endIndex - 1
      focusOn(idx)
      lastIndexOfBlock = idx
      lo = (idx - focusStart) & 31
      endLo = spire.math.max(startIndex - focusStart - lastIndexOfBlock, 0)
    } else {
      lastIndexOfBlock = 0
      lo = 0
      endLo = 0
      display0 = new Leaf(1)
      display1 = new Node(3)
      display1.update(0, display0)
      display1 = withComputedSizes(display1, 1)
    }
  }

  final def hasNext: Boolean = _hasNext

  def next(): A = {
    // Keep method size under 35 bytes, so that it can be JIT-inlined
    if (_hasNext) {
      val res = display0(lo)
      lo -= 1
      if (lo >= endLo)
        res
      else {
        gotoPrevBlock()
        res
      }
    } else
      throw new NoSuchElementException("reached iterator end")
  }

  final private[Immutable] def gotoPrevBlock(): Unit = {
    val newBlockIndex = lastIndexOfBlock - 32
    if (focusStart <= newBlockIndex) {
      val _focusStart = focusStart
      val newBlockIndexInFocus = newBlockIndex - _focusStart
      gotoPrevBlockStart(
        newBlockIndexInFocus,
        newBlockIndexInFocus ^ (lastIndexOfBlock - _focusStart)
      )
      lastIndexOfBlock = newBlockIndex
      lo = 31
      endLo = spire.math.max(startIndex - focusStart - focus, 0)
    } else if (startIndex < focusStart) {
      val newIndex = focusStart - 1
      focusOn(newIndex)
      lastIndexOfBlock = newIndex
      lo = (newIndex - focusStart) & 31
      endLo = spire.math.max(startIndex - focusStart - lastIndexOfBlock, 0)
    } else {
      _hasNext = false
    }
  }

}
