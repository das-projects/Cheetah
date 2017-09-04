package Cheetah.Immutable

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable
import scala.{specialized => sp}
import scala.{Vector => Vec}


final class VectorBuilder[@sp A]
  extends mutable.Builder[A, Vector[A]]
    with VectorPointer[A @uncheckedVariance] {

  display0 = new Leaf(32)
  depth = 1

  private var blockIndex: Int = 0
  private var lo: Int = 0
  private var acc: Vector[A] = _

  def finalise: Vector[A] = result

  def +=(elem: A): this.type = {
    if (lo >= 32) {
      val newBlockIndex = blockIndex + 32
      gotoNextBlockStartWritable(newBlockIndex, newBlockIndex ^ blockIndex)
      blockIndex = newBlockIndex
      lo = 0
    }
    display0.update(lo, elem)
    lo += 1
    this

  }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    if (xs.nonEmpty)
      xs match {
        case thatVec: Vector[A] =>
          if (thatVec.size > (1 << 10))
            if (endIndex != 0) {
              acc = this.result() ++ xs
              this.clearCurrent()
            } else if (acc != null)
              acc = acc ++ thatVec
            else
              acc = thatVec
          else {
            super.++=(xs)
          }
        case _ => super.++=(xs)
      }
    this
  }

  def result: Vector[A] = {
    val current = currentResult()
    val resultVector =
      if (acc == null)
        current
      else
        acc ++ current
    resultVector
  }

  def clear: Unit = {
    clearCurrent()
    acc = null
  }

  private[Immutable] def endIndex = {
    var sz = blockIndex + lo
    if (acc != null)
      sz += acc.endIndex

    sz
  }

  private def currentResult(): Vector[A] = {
    val size = blockIndex + lo
    if (size == 0)
      Vector.empty
    else {
      val resultVector = new Vector[A](size)
      resultVector.initFrom(this)
      resultVector.display0 = copyOf(resultVector.display0, lo, lo)
      val _depth = depth
      if (_depth > 1) {
        resultVector.copyDisplays(_depth, size - 1)
        resultVector.stabilizeDisplayPath(_depth, size - 1)
      }
      resultVector.gotoPos(0, size - 1)
      resultVector.initFocus(0, 0, size, _depth, 0)
      resultVector
    }
  }

  private def clearCurrent(): Unit = {
    display0 = new Leaf(32)
    display1 = _
    display2 = _
    display3 = _
    display4 = _
    display5 = _
    display6 = _
    depth = 1
    blockIndex = 0
    lo = 0
  }
}
