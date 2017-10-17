package Cheetah.Immutable

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.TraversableOnce
import scala.reflect.ClassTag
import scala.{Vector => ScalaVector}

class VectorBuilder[A](implicit val ct: ClassTag[A]) extends VectorPointer[A @uncheckedVariance] {

  display0 = new Leaf(32)
  display1 = new Node(33)
  display1.update(0, display0)
  //display1 = withComputedSizes(display1, 1)
  depth = 1

  private var blockIndex: Int = 0
  private var lo: Int = 0
  private var acc: Vector[A] = _

  private[Immutable] def endIndex = {
    var size = blockIndex + lo
    if (acc != null) size += acc.endIndex
    size
  }

  /** Adds a single element to the builder.
    *  @param elem the element to be added.
    *  @return the builder itself.
    */

  def +=(elem: A): VectorBuilder[A] = {
    if (lo >= 32) {
      val newBlockIndex: Int = blockIndex + 32
      gotoNextBlockStartWritable(newBlockIndex, newBlockIndex ^ blockIndex)
      blockIndex = newBlockIndex
      lo = 0
    }
    display0.update(lo, elem)
    lo += 1
    this
  }

  /** ${Add}s two or more elements to this $coll.
    *
    *  @param elem1 the first element to $add.
    *  @param elem2 the second element to $add.
    *  @param elems the remaining elements to $add.
    *  @return the $coll itself
    */

  def +=(elem1: A, elem2: A, elems: Vector[A]): VectorBuilder[A] = this += elem1 += elem2 ++= elems

  /** ${Add}s all elements produced by a TraversableOnce to this $coll.
    *
    *  @param xs   the TraversableOnce producing the elements to $add.
    *  @return  the $coll itself.
    */

  def ++=(xs: Vector[A]): VectorBuilder[A] = {
    @tailrec def loopVector(xs: Vector[A]): Unit = {
      if (xs.nonEmpty) {
        this += xs.head
        loopVector(xs.tail)
      }
    }

    if (xs.nonEmpty)
          if (xs.size > (1 << 10)) {
            if (endIndex != 0) {
              acc = this.result() ++ xs
              this.clearCurrent()
            } else {
              if (acc != null)
                acc = acc ++ xs
              else
                acc = xs
            }
          } else {
            loopVector(xs)
            acc = this.result()
            this.clearCurrent()
          }
    this
  }

  /** Clears the contents of this builder.
    *  After execution of this method the builder will contain no elements.
    */
  def clear(): Unit = {
    clearCurrent()
    acc = null
  }

  private def clearCurrent(): Unit = {

    display0 = new Leaf(32)
    display1 = new Node(33)
    display2 = null
    display3 = null
    display4 = null
    display5 = null
    display6 = null
    display7 = null

    display1.update(0, display0)
    display1 = withComputedSizes(display1, 1)
    depth = 1
    blockIndex = 0
    lo = 0
  }

  /** Produces a collection from the added elements.  This is a terminal operation:
    *  the builder's contents are undefined after this operation, and no further
    *  methods should be called.
    *
    *  @return a collection containing the elements added to this builder.
    */
  def result(): Vector[A] = {
    val current: Vector[A] = currentResult()
    val resultVector: Vector[A] =
      if (acc == null)
        current
      else
        acc ++ current
    resultVector
  }

  private def currentResult(): Vector[A] = {
    val size: Int = blockIndex + lo
    if (size == 0)
      Vector.empty
    else {
      val resultVector: Vector[A] = new Vector[A](size)
      resultVector.initFrom(this)
      resultVector.display0 = copyOf(resultVector.display0, lo, lo)
      val _depth: Int = depth
      if (_depth >= 1) {
        resultVector.copyDisplays(_depth, size - 1)
        resultVector.stabilizeDisplayPath(_depth, size - 1)
      }
      resultVector.gotoPos(0, size - 1)
      resultVector.initFocus(0, 0, size, _depth, 0)
      resultVector
    }
  }

  /** Gives a hint how many elements are expected to be added
    *  when the next `result` is called. Some builder classes
    *  will optimize their representation based on the hint. However,
    *  builder implementations are still required to work correctly even if the hint is
    *  wrong, i.e. a different number of elements is added.
    *
    *  @param size  the hint how many elements will be added.
    */
  def sizeHint(size: Int) {}

  /** Gives a hint that one expects the `result` of this builder
    *  to have the same size as the given collection, plus some delta. This will
    *  provide a hint only if the collection is known to have a cheap
    *  `size` method, which is determined by calling `sizeHint`.
    *
    *  Some builder classes will optimize their representation based on the hint. However,
    *  builder implementations are still required to work correctly even if the hint is
    *  wrong, i.e. a different number of elements is added.
    *
    *  @param coll  the collection which serves as a hint for the result's size.
    */
  def sizeHint(coll: VectorIterator[A]) {
    coll.sizeHintIfCheap match {
      case -1 =>
      case n => sizeHint(n)
    }
  }

  /** Gives a hint that one expects the `result` of this builder
    *  to have the same size as the given collection, plus some delta. This will
    *  provide a hint only if the collection is known to have a cheap
    *  `size` method. Currently this is assumed to be the case if and only if
    *  the collection is of type `IndexedSeqLike`.
    *  Some builder classes
    *  will optimize their representation based on the hint. However,
    *  builder implementations are still required to work correctly even if the hint is
    *  wrong, i.e. a different number of elements is added.
    *
    *  @param coll  the collection which serves as a hint for the result's size.
    *  @param delta a correction to add to the `coll.size` to produce the size hint.
    */
  def sizeHint(coll: VectorIterator[A], delta: Int) {
    coll.sizeHintIfCheap match {
      case -1 =>
      case n => sizeHint(n + delta)
    }
  }

  /** Gives a hint how many elements are expected to be added
    *  when the next `result` is called, together with an upper bound
    *  given by the size of some other collection. Some builder classes
    *  will optimize their representation based on the hint. However,
    *  builder implementations are still required to work correctly even if the hint is
    *  wrong, i.e. a different number of elements is added.
    *
    *  @param size  the hint how many elements will be added.
    *  @param boundColl  the bounding collection. If it is
    *                       an IndexedSeqLike, then sizes larger
    *                       than collection's size are reduced.
    */
  def sizeHintBounded(size: Int, boundColl: VectorIterator[A]) {
    boundColl.sizeHintIfCheap match {
      case -1 => ()
      case n => sizeHint(size min n)
    }
  }

  /** Creates a new builder by applying a transformation function to
    *  the results of this builder.
    *  @param  f     the transformation function.
    *  @tparam B the type of collection returned by `f`.
    *  @return a new builder which is the same as the current builder except
    *          that a transformation function is applied to this builder's result.
    *
    *  @note The original builder should no longer be used after `mapResult` is called.
    */

  def mapResult[B: ClassTag](f: Vector[A] => Vector[B]): VectorBuilder[B] =
    new VectorBuilder[B]{
      val self: VectorBuilder[A] = VectorBuilder.this
      def +=(x: A): this.type = { self += x; this }
      override def clear(): Unit = self.clear()
      def ++=(xs: Vector[A]): this.type = { self ++= xs; this }
      override def sizeHint(size: Int): Unit = self.sizeHint(size)
      override def result(): Vector[B] = f(self.result())
    }

}
