package Cheetah
package collection
package immutable

import spire.ClassTag

import scala.annotation.tailrec
import scala.compat.Platform
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, IndexedSeqFactory}
import scala.collection.{AbstractIterator, GenTraversableOnce, mutable}
import scala.{specialized => sp}

object RRBVector extends scala.collection.generic.IndexedSeqFactory[RRBVector] {

  //  @inline private[immutable] final val compileAssertions = false

  def newBuilder[A]: mutable.Builder[A, RRBVector[A]] =
    new RRBVectorBuilder[A]()

/*
  implicit def canBuildFrom[A]
  : scala.collection.generic.CanBuildFrom[Coll, A, RRBVector[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
*/

  implicit def canBuildFrom[A]: CanBuildFrom[RRBVector[_], A, RRBVector[A]] =
    new CanBuildFrom[RRBVector[_], A, RRBVector[A]] {
      def apply = new RRBVectorBuilder[A]

      override def apply(from: RRBVector[_]): A = ???
    }

  lazy private val EMPTY_VECTOR = new RRBVector[Nothing](0)

  override def empty[A]: RRBVector[A] = EMPTY_VECTOR

  final lazy private[immutable] val emptyTransientBlock = new Array[AnyRef](2)
}

final class RRBVector[+A](override private[immutable] val endIndex: Int)
  extends scala.collection.AbstractSeq[A]
    with scala.collection.immutable.IndexedSeq[A]
    with scala.collection.generic.GenericTraversableTemplate[A, RRBVector]
    with scala.collection.IndexedSeqLike[A, RRBVector[A]]
    with RRBVectorPointer[A @uncheckedVariance]
    with Serializable { self =>

  private[immutable] var transient: Boolean = false

  override def companion: scala.collection.generic.GenericCompanion[RRBVector] = RRBVector

  def length: Int = endIndex

  override def lengthCompare(len: Int): Int = endIndex - len

  override def par = new ParRRBVector[A](this)

  override def iterator: RRBVectorIterator[A] = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }
    val it = new RRBVectorIterator[A](0, endIndex)
    it.initIteratorFrom(this)
    it
  }

  override def reverseIterator: RRBVectorReverseIterator[A] = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }
    val rit = new RRBVectorReverseIterator[A](0, endIndex)
    rit.initIteratorFrom(this)
    rit
  }

  def apply(index: Int): A = {
    val _focusStart = this.focusStart
    if (_focusStart.<=(index).&&(index.<(focusEnd))) {
      val indexInFocus = index.-(_focusStart)
      getElem(indexInFocus, indexInFocus.^(focus))
    } else if ((0).<=(index).&&(index.<(endIndex)))
      getElementFromRoot(index)
    else
      throw new IndexOutOfBoundsException(index.toString)
  }

  override def :+[B >: A, That](elem: B)(
    implicit bf: CanBuildFrom[RRBVector[A], B, That]): That =
    if (bf.eq(IndexedSeq.ReusableCBF)) {
      val _endIndex = this.endIndex
      if (_endIndex.!=(0)) {
        val resultVector = new RRBVector[B](_endIndex + 1)
        resultVector.transient = this.transient
        resultVector.initWithFocusFrom(this)
        resultVector.append(elem, _endIndex)
        resultVector.asInstanceOf[That]
      } else
        createSingletonVector(elem).asInstanceOf[That]
    } else
      super.:+(elem)(bf)

  override def +:[B >: A, That](elem: B)(
    implicit bf: CanBuildFrom[RRBVector[A], B, That]): That =
    if (bf.eq(IndexedSeq.ReusableCBF)) {
      val _endIndex = this.endIndex
      if (_endIndex != 0) {
        val resultVector = new RRBVector[B](_endIndex.+(1))
        resultVector.transient = this.transient
        resultVector.initWithFocusFrom(this)
        resultVector.prepend(elem)
        resultVector.asInstanceOf[That]
      } else
        createSingletonVector(elem).asInstanceOf[That]
    } else
      super.:+(elem)(bf)

  override def isEmpty: Boolean = this.endIndex == 0

  override def head: A =
    if (this.endIndex != 0)
      apply(0)
    else
      throw new UnsupportedOperationException("empty.head")

  override def take(n: Int): RRBVector[A] =
    if (n.<=(0))
      RRBVector.empty
    else if (n.<(endIndex))
      takeFront0(n)
    else
      this

  override def drop(n: Int): RRBVector[A] =
    if (n.<=(0))
      this
    else if (n.<(endIndex))
      dropFront0(n)
    else
      RRBVector.empty

  override def dropRight(n: Int): RRBVector[A] =
    if (n.<=(0))
      this
    else if (n.<(endIndex))
      takeFront0(endIndex.-(n))
    else
      RRBVector.empty

  override def takeRight(n: Int): RRBVector[A] =
    if (n.<=(0))
      RRBVector.empty
    else if (n.<(endIndex))
      dropFront0(endIndex.-(n))
    else
      this

  override def slice(from: Int, until: Int): RRBVector[A] =
    take(until).drop(from)

  override def splitAt(n: Int): scala.Tuple2[RRBVector[A], RRBVector[A]] =
    scala.Tuple2(take(n), drop(n))

  override def ++[B >: A, That](that: GenTraversableOnce[B])(
    implicit bf: CanBuildFrom[RRBVector[A], B, That]): That =
    if (bf.eq(IndexedSeq.ReusableCBF))
      if (that.isEmpty)
        this.asInstanceOf[That]
      else that match {
        case thatVec: RRBVector[B] =>
          if (this.endIndex == 0)
            thatVec.asInstanceOf[That]
          else {
            val newVec = new RRBVector(this.endIndex.+(thatVec.endIndex))
            newVec.initWithFocusFrom(this)
            newVec.transient = this.transient
            newVec.concatenate(this.endIndex, thatVec)
            newVec.asInstanceOf[That]
          }
        case _ => super.++(that.seq)
      }
    else
      super.++(that.seq)

  override def tail: RRBVector[A] =
    if (this.endIndex.!=(0))
      this.drop(1)
    else
      throw new UnsupportedOperationException("empty.tail")

  override def last: A =
    if (this.endIndex.!=(0))
      this.apply(this.length.-(1))
    else
      throw new UnsupportedOperationException("empty.last")

  override def init: RRBVector[A] =
    if (this.endIndex.!=(0))
      dropRight(1)
    else
      throw new UnsupportedOperationException("empty.init")

  private[immutable] def append[ B >: A](elem: B, _endIndex: Int): scala.Unit = {
    if (focusStart.+(focus).^(_endIndex.-(1)).>=(32))
      normalizeAndFocusOn(_endIndex.-(1))

    val elemIndexInBlock = _endIndex.-(focusStart).&(31)
    if (elemIndexInBlock.!=(0))
      appendOnCurrentBlock(elem, elemIndexInBlock)
    else
      appendBackNewBlock(elem, elemIndexInBlock)
  }

  private def appendOnCurrentBlock[ B >: A](elem: B,
                                      elemIndexInBlock: Int): scala.Unit = {
    focusEnd = endIndex
    val d0 = new Node(elemIndexInBlock.+(1))
    System.arraycopy(display0, 0, d0, 0, elemIndexInBlock)
    d0.update(elemIndexInBlock, elem.asInstanceOf[AnyRef])
    display0 = d0.asInstanceOf[Node]
    makeTransientIfNeeded()
  }

  private def appendBackNewBlock[ B >: A](elem: B,
                                    elemIndexInBlock: Int): scala.Unit = {
    val oldDepth = depth
    val newRelaxedIndex = endIndex.-(1).-(focusStart).+(focusRelax)
    val focusJoined = focus.|(focusRelax)
    val xor = newRelaxedIndex.^(focusJoined)
    val _transient = transient
    setupNewBlockInNextBranch(xor, _transient)
    if (oldDepth == depth) {
      var i =
        if (xor < (1 << 10))
          2
        else if (xor < (1 << 15))
          3
        else if (xor < (1 << 20))
          4
        else if (xor < (1 << 25))
          5
        else if (xor < (1 << 30))
          6
        else if (xor < (1 << 35))
          7
        else
          7
      if (i < oldDepth) {
        val _focusDepth = focusDepth
        var display: Node = i match {
          case 2 => display2
          case 3 => display3
          case 4 => display4
          case 5 => display5
          case 6 => display6
        }
        do {
          val displayLen = display.length - 1
          val newSizes: Array[Int] =
            if (i.>=(_focusDepth))
              makeTransientSizes(display(displayLen).asInstanceOf[Array[Int]],
                displayLen - 1)
            else
              null
          val newDisplay = new Node(display.length)
          System.arraycopy(display, 0, newDisplay, 0, displayLen - 1)
          if (i >= _focusDepth)
            newDisplay.update(displayLen, newSizes)

          i match {
            case 2 =>
              display2 = newDisplay
              display = display3
            case 3 =>
              display3 = newDisplay
              display = display4
            case 4 =>
              display4 = newDisplay
              display = display5
            case 5 =>
              display5 = newDisplay
              display = display6
            case 6 => display6 = newDisplay
          }
          i.+=(1)
        } while (i < oldDepth)
      }
    }
    if (oldDepth == focusDepth)
      initFocus(endIndex - 1, 0, endIndex, depth, 0)
    else
      initFocus(endIndex.-(1),
        endIndex.-(1),
        endIndex,
        1,
        newRelaxedIndex.&(-32))
    display0.update(elemIndexInBlock, elem.asInstanceOf[Node])
    transient = true
  }
  private[immutable] def prepend[ B >: A](elem: B): scala.Unit = {

    if (focusStart.!=(0).||(focus.&(-32).!=(0)))
      normalizeAndFocusOn(0)

    val d0 = display0
    if (d0.length < 32)
      prependOnCurrentBlock(elem, d0.asInstanceOf[Array[B]])
    else
      prependFrontNewBlock(elem)
  }

  private def prependOnCurrentBlock[ B >: A](elem: B,
                                                oldD0: Array[B]): scala.Unit = {
    val newLen = oldD0.length + 1
    focusEnd = newLen
    val newD0 = new Array(newLen).asInstanceOf[Array[B]]
    newD0.update(0, elem.asInstanceOf[B])
    System.arraycopy(oldD0, 0, newD0, 1, newLen.-(1))
    display0 = newD0.asInstanceOf[Node]
    makeTransientIfNeeded()
  }
  private def prependFrontNewBlock[ B >: A](elem: B): scala.Unit = {
    var currentDepth = focusDepth
    if (currentDepth.==(1))
      currentDepth.+=(1)
    else
      ()
    var display = currentDepth match {
      case 1 =>
        currentDepth = 2
        display1
      case 2 => display1
      case 3 => display2
      case 4 => display3
      case 5 => display4
      case 6 => display5
      case 7 => display6
    }
    while (display.!=(null).&&(display.length.==(33))) {
      currentDepth.+=(1)
      currentDepth match {
        case 2 => display = display1
        case 3 => display = display2
        case 4 => display = display3
        case 5 => display = display4
        case 6 => display = display5
        case 7 => display = display6
        case _ => throw new IllegalStateException()
      }
    }
    val oldDepth = depth
    val _transient = transient
    setupNewBlockInInitBranch(currentDepth, _transient)
    if (oldDepth.==(depth)) {
      var i = currentDepth
      if (i.<(oldDepth)) {
        val _focusDepth = focusDepth
        var display: Node = i match {
          case 2 => display2
          case 3 => display3
          case 4 => display4
          case 5 => display5
          case 6 => display6
        }
        do {
          val displayLen = display.length.-(1)
          val newSizes: Array[Int] =
            if (i.>=(_focusDepth))
              makeTransientSizes(display(displayLen).asInstanceOf[Array[Int]],
                1)
            else
              null
          val newDisplay = new Node(display.length)
          System.arraycopy(display, 0, newDisplay, 0, displayLen.-(1))
          if (i.>=(_focusDepth))
            newDisplay.update(displayLen, newSizes)

          i match {
            case 2 =>
              display2 = newDisplay
              display = display3
            case 3 =>
              display3 = newDisplay
              display = display4
            case 4 =>
              display4 = newDisplay
              display = display5
            case 5 =>
              display5 = newDisplay
              display = display6
            case 6 => display6 = newDisplay
          }
          i += 1
        } while (i.<(oldDepth))
      }
    }
    initFocus(0, 0, 1, 1, 0)
    display0.update(0, elem.asInstanceOf[Node])
    transient = true
  }

  private def createSingletonVector[ B >: A](elem: B): RRBVector[B] = {
    val resultVector = new RRBVector[B](1)
    resultVector.initSingleton(elem)
    resultVector
  }

  private[immutable] def normalizeAndFocusOn(index: Int): scala.Unit = {
    if (transient) {
      normalize(depth)
      transient = false
    }
    focusOn(index)
  }

  private[immutable] def makeTransientIfNeeded(): scala.Unit = {
    val _depth = depth
    if (_depth.>(1).&&(transient.`unary_!`)) {
      copyDisplaysAndNullFocusedBranch(_depth, focus.|(focusRelax))
      transient = true
    }
  }
  private[immutable] def concatenate[ B >: A](currentSize: Int,
                                                 that: RRBVector[B]): scala.Unit = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }

    if (that.transient) {
      that.normalize(that.depth)
      that.transient = false
    }

    this.focusOn(currentSize.-(1))
    math.max(this.depth, that.depth) match {
      case 1 =>
        val concat = rebalancedLeafs(display0, that.display0, isTop = true)
        initFromRoot(concat.asInstanceOf[Node], if(endIndex <= 32) 1 else 2)

      case 2 =>
        var d0: Node = null
        var d1: Node = null
        if (that.focus.&(-32).==(0)) {
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Node]
        } else {
          if (that.display1 != null)
            d1 = that.display1
          if (d1 == null)
            d0 = that.display0.asInstanceOf[Node]
          else
            d0 = d1(0).asInstanceOf[Node]

          var concat = rebalancedLeafs(this.display0, d0, isTop = false)
          concat = rebalanced(this.display1, concat, that.display1, 2)
          if (concat.length.==(2))
            initFromRoot(concat(0).asInstanceOf[Node], 2)
          else
            initFromRoot(withComputedSizes(concat, 3), 3)
        }

      case 3 =>
        var d0: Node = null
        var d1: Node = null
        var d2: Node = null
        if (that.focus.&(-32).==(0)) {
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Node]
        } else {
          if (that.display2.!=(null))
            d2 = that.display2

          if (d2.==(null))
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1.==(null))
            d0 = that.display0.asInstanceOf[Node]
          else
            d0 = d1(0).asInstanceOf[Node]
        }
        var concat: Node =
          rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, that.display2, 3)
        if (concat.length.==(2))
          initFromRoot(concat(0).asInstanceOf[Node], 3)
        else
          initFromRoot(withComputedSizes(concat, 4), 4)
      case 4 =>
        var d0: Node = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null
        if (that.focus.&(-32).==(0)) {
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Node]
        } else {
          if (that.display3.!=(null))
            d3 = that.display3
          else
            ()
          if (d3.==(null))
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2.==(null))
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1.==(null))
            d0 = that.display0.asInstanceOf[Node]
          else
            d0 = d1(0).asInstanceOf[Node]
        }
        var concat: Node =
          rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, that.display3, 4)
        if (concat.length.==(2))
          initFromRoot(concat(0).asInstanceOf[Node], 4)
        else
          initFromRoot(withComputedSizes(concat, 5), 5)
      case 5 =>
        var d0: Node = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null
        var d4: Node = null
        if (that.focus.&(-32).==(0)) {
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Node]
        } else {
          if (that.display4.!=(null))
            d4 = that.display4
          else
            ()
          if (d4.==(null))
            d3 = that.display3
          else
            d3 = d4(0).asInstanceOf[Node]
          if (d3.==(null))
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2.==(null))
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1.==(null))
            d0 = that.display0.asInstanceOf[Node]
          else
            d0 = d1(0).asInstanceOf[Node]
        }
        var concat: Node =
          rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, that.display4, 5)
        if (concat.length.==(2))
          initFromRoot(concat(0).asInstanceOf[Node], 5)
        else
          initFromRoot(withComputedSizes(concat, 6), 6)
      case 6 =>
        var d0: Node = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null
        var d4: Node = null
        var d5: Node = null
        if (that.focus.&(-32).==(0)) {
          d5 = that.display5
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Node]
        } else {
          if (that.display5.!=(null))
            d5 = that.display5
          else
            ()
          if (d5.==(null))
            d4 = that.display4
          else
            d4 = d5(0).asInstanceOf[Node]
          if (d4.==(null))
            d3 = that.display3
          else
            d3 = d4(0).asInstanceOf[Node]
          if (d3.==(null))
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2.==(null))
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1.==(null))
            d0 = that.display0.asInstanceOf[Node]
          else
            d0 = d1(0).asInstanceOf[Node]
        }
        var concat: Node =
          rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, d4, 5)
        concat = rebalanced(this.display5, concat, that.display5, 6)
        if (concat.length.==(2))
          initFromRoot(concat(0).asInstanceOf[Node], 6)
        else
          initFromRoot(withComputedSizes(concat, 7), 7)
      case 7 =>
        var d0: Node = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null
        var d4: Node = null
        var d5: Node = null
        var d6: Node = null
        if (that.focus.&(-32).==(0)) {
          d6 = that.display6
          d5 = that.display5
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Node]
        } else {
          if (that.display6.!=(null))
            d6 = that.display6

          if (d6.==(null))
            d5 = that.display5
          else
            d5 = d6(0).asInstanceOf[Node]
          if (d5.==(null))
            d4 = that.display4
          else
            d4 = d5(0).asInstanceOf[Node]
          if (d4.==(null))
            d3 = that.display3
          else
            d3 = d4(0).asInstanceOf[Node]
          if (d3.==(null))
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2.==(null))
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1.==(null))
            d0 = that.display0.asInstanceOf[Node]
          else
            d0 = d1(0).asInstanceOf[Node]
        }
        var concat: Node = rebalancedLeafs(this.display0, d0, isTop = false)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, d4, 5)
        concat = rebalanced(this.display5, concat, d5, 6)
        concat = rebalanced(this.display6, concat, that.display6, 7)
        if (concat.length.==(2))
          initFromRoot(concat(0).asInstanceOf[Node], 7)
        else
          initFromRoot(withComputedSizes(concat, 8), 8)
      case _ => throw new IllegalStateException()
    }
  }
  private def rebalanced(displayLeft: Node,
                         concat: Node,
                         displayRight: Node,
                         currentDepth: Int): Node = {
    val leftLength =
      if (displayLeft.==(null))
        0
      else
        displayLeft.length.-(1)

    val concatLength =
      if (concat.==(null))
        0
      else
        concat.length.-(1)

    val rightLength =
      if (displayRight.==(null))
        0
      else
        displayRight.length.-(1)

    val branching =
      computeBranching(displayLeft, concat, displayRight, currentDepth)

    val top = new Node(branching>>10
      + (if(branching.&(1023) == 0) 1 else 2)
    )

    var mid = new Node(
      if (branching.>>(10).==(0))
        branching.+(31).>>(5) + 1 else 33)

    var bot: Node = null
    var iSizes = 0
    var iTop = 0
    var iMid = 0
    var iBot = 0
    var i = 0
    var j = 0
    var d = 0

    var currentDisplay: Node = null
    var displayEnd = 0

    do {
      d match {
        case 0 =>
          if (displayLeft.!=(null)) {
            currentDisplay = displayLeft
            if (concat.==(null))
              displayEnd = leftLength
            else
              displayEnd = leftLength.-(1)
          }
        case 1 =>
          if (concat.==(null))
            displayEnd = 0
          else {
            currentDisplay = concat
            displayEnd = concatLength
          }
          i = 0
        case 2 =>
          if (displayRight.!=(null)) {
            currentDisplay = displayRight
            displayEnd = rightLength
            if (concat.==(null))
              i = 0
            else
              i = 1
          }
      }
      while (i < displayEnd) {
        val displayValue = currentDisplay(i)
          .asInstanceOf[Node]
        val displayValueEnd =
          if (currentDepth.==(2))
            displayValue.length
          else
            displayValue.length.-(1)
        if (iBot.|(j).==(0).&&(displayValueEnd.==(32))) {
          if (currentDepth.!=(2).&&(bot.!=(null))) {
            withComputedSizes(bot, currentDepth.-(1))
            bot = null
          }
          mid.update(iMid, displayValue)
          i.+=(1)
          iMid.+=(1)
          iSizes.+=(1)
        } else {
          val numElementsToCopy = math.min(displayValueEnd - j, 32 - iBot)

          if (iBot == 0) {
            if (currentDepth != 2 && bot != null)
              withComputedSizes(bot, currentDepth.-(1))

            bot = new Node(
              math.min(branching -(iTop << 10) - (iMid << 5), 32)
                + (if (currentDepth == 2) 0 else 1)
            )
            mid.update(iMid, bot)
          }

          System.arraycopy(displayValue, j, bot, iBot, numElementsToCopy)
          j += numElementsToCopy
          iBot += numElementsToCopy

          if (j == displayValueEnd) {
            i += 1
            j = 0
          }

          if (iBot == 32) {
            iMid += 1
            iBot = 0
            iSizes += 1
            if (currentDepth != 2 && bot != null)
              withComputedSizes(bot, currentDepth.-(1))
          }
        }
        if (iMid == 32) {
          top.update(iTop,
            if (currentDepth == 1)
              withComputedSizes1(mid)
            else
              withComputedSizes(mid, currentDepth))
          iTop += 1
          iMid = 0
          val remainingBranches =
            branching - (iTop << 5 | (iMid << 5) | iBot)
          if (remainingBranches > 0)
            mid = new Node(
              if ((remainingBranches >> 10) == 0)
                remainingBranches.+(63).>>(5) else 33)
          else
            mid = null
        }
      }
      d += 1
    } while (d < 3)
    if (currentDepth != 2 && bot != null)
      withComputedSizes(bot, currentDepth - 1)

    if (mid != null)
      top.update(iTop,
        if (currentDepth == 1)
          withComputedSizes1(mid)
        else
          withComputedSizes(mid, currentDepth))
    top
  }

  private def rebalancedLeafs[A](displayLeft: Node,
                                           displayRight: Node,
                                           isTop: Boolean): Node = {
    val leftLength = displayLeft.length
    val rightLength = displayRight.length
    if (leftLength == 32) {
      val top = new Array(3).asInstanceOf[Node]
      top.update(0, displayLeft)
      top.update(1, displayRight)
      top
    } else if (leftLength + rightLength <= 32) {

      val mergedDisplay = new Array(leftLength.+(rightLength)).asInstanceOf[Node]
      System.arraycopy(displayLeft, 0, mergedDisplay, 0, leftLength)
      System.arraycopy(displayRight, 0, mergedDisplay, leftLength, rightLength)

      if (isTop)
        mergedDisplay
      else {
        val top = new Array(2).asInstanceOf[Node]
        top.update(0, mergedDisplay)
        top
      }
    } else {
      val top = new Array(3).asInstanceOf[Node]
      val arr0 = new Array(32).asInstanceOf[Node]
      val arr1 = new Array(leftLength.+(rightLength).-(32)).asInstanceOf[Node]
      top.update(0, arr0)
      top.update(1, arr1)

      System.arraycopy(displayLeft, 0, arr0, 0, leftLength)
      System.arraycopy(displayRight, 0, arr0, leftLength, 32 - leftLength)
      System.arraycopy(displayRight, 32 - leftLength, arr1, 0, rightLength - 32 + leftLength)
      top
    }
  }
  private def computeBranching(displayLeft: Node,
                               concat: Node,
                               displayRight: Node,
                               currentDepth: Int) = {
    val leftLength =
      if (displayLeft == null)
        0
      else
        displayLeft.length - 1
    val concatLength =
      if (concat == null)
        0
      else
        concat.length - 1
    val rightLength =
      if (displayRight == null)
        0
      else
        displayRight.length - 1
    var branching = 0
    if (currentDepth == 1) {
      branching = leftLength.+(concatLength).+(rightLength)

      if (leftLength != 0)
        branching -= 1

      if (rightLength.!=(0))
        branching.-=(1)

    } else {
      var i = 0

      while (i < leftLength - 1) {
        branching += displayLeft(i).asInstanceOf[Node].length
        i += 1
      }
      i = 0
      while (i < concatLength) {
        branching += concat(i).asInstanceOf[Node].length
        i += 1
      }
      i = 1
      while (i < rightLength) {
        branching += displayRight(i).asInstanceOf[Node].length
        i += 1
      }
      if (currentDepth != 2) {
        branching -= leftLength + concatLength + rightLength

        if (leftLength.!=(0))
          branching += 1

        if (rightLength.!=(0))
          branching += 1
      }
    }
    branching
  }
  private def takeFront0(n: Int): RRBVector[A] = {

    if (transient) {
      normalize(depth)
      transient = false
    }

    val vec = new RRBVector[A](n)

    vec.initWithFocusFrom(this)

    if (depth > 1) {
      vec.focusOn(n - 1)
      val d0len = vec.focus.&(31).+(1)
      if (d0len != 32) {
        val d0 = new Array(d0len).asInstanceOf[Node]
        System.arraycopy(vec.display0, 0, d0, 0, d0len)
        vec.display0 = d0
      }

      val cutIndex = vec.focus.|(vec.focusRelax)
      vec.cleanTopTake(cutIndex)
      vec.focusDepth = math.min(vec.depth, vec.focusDepth)

      if (vec.depth > 1) {
        vec.copyDisplays(vec.focusDepth, cutIndex)
        var i = vec.depth
        var offset = 0
        var display: Node = null

        while (i > vec.focusDepth) {
          i match {
            case 2 => display = vec.display1
            case 3 => display = vec.display2
            case 4 => display = vec.display3
            case 5 => display = vec.display4
            case 6 => display = vec.display5
            case 7 => display = vec.display6
          }
          val oldSizes = display(display.length.-(1))
            .asInstanceOf[Array[Int]]
          val newLen = vec.focusRelax.>>((5).*(i.-(1))).&(31).+(1)
          val newSizes = new Array[Int](newLen)
          System.arraycopy(oldSizes, 0, newSizes, 0, newLen.-(1))
          newSizes.update(newLen.-(1), n.-(offset))
          if (newLen.>(1))
            offset.+=(newSizes(newLen.-(2)))

          val newDisplay = new Node(newLen.+(1))
          System.arraycopy(display, 0, newDisplay, 0, newLen)
          newDisplay.update(newLen.-(1), null)
          newDisplay.update(newLen, newSizes)
          i match {
            case 2 => vec.display1 = newDisplay
            case 3 => vec.display2 = newDisplay
            case 4 => vec.display3 = newDisplay
            case 5 => vec.display4 = newDisplay
            case 6 => vec.display5 = newDisplay
            case 7 => vec.display6 = newDisplay
          }
          i -= 1
        }
        vec.stabilizeDisplayPath(vec.depth, cutIndex)
        vec.focusEnd = n
      } else
        vec.focusEnd = n
    } else if (n != 32) {
      val d0 = new Array(n).asInstanceOf[Node]
      System.arraycopy(vec.display0, 0, d0, 0, n)
      vec.display0 = d0
      vec.initFocus(0, 0, n, 1, 0)
    }
    vec
  }
  private def dropFront0(n: Int): RRBVector[A] = {

    if (transient) {
      normalize(depth)
      transient = false
    }

    val vec = new RRBVector[A](this.endIndex.-(n))

    vec.initWithFocusFrom(this)

    if (vec.depth.>(1)) {
      vec.focusOn(n)
      val cutIndex = vec.focus.|(vec.focusRelax)
      val d0Start = cutIndex.&(31)
      if (d0Start.!=(0)) {
        val d0len = vec.display0.length.-(d0Start)
        val d0 = new Array(d0len).asInstanceOf[Node]
        System.arraycopy(vec.display0, d0Start, d0, 0, d0len)
        vec.display0 = d0
      }

      vec.cleanTopDrop(cutIndex)

      if (vec.depth.>(1)) {
        var i = 2
        var display = vec.display1
        while (i.<=(vec.depth)) {
          val splitStart = cutIndex.>>((5).*(i.-(1))).&(31)
          val newLen = display.length.-(splitStart).-(1)
          val newDisplay = new Node(newLen.+(1))
          System.arraycopy(display, splitStart.+(1), newDisplay, 1, newLen.-(1))
          i match {
            case 2 => {
              newDisplay.update(0, vec.display0)
              vec.display1 = withComputedSizes(newDisplay, 2)
              display = vec.display2
            }
            case 3 => {
              newDisplay.update(0, vec.display1)
              vec.display2 = withComputedSizes(newDisplay, 3)
              display = vec.display3
            }
            case 4 => {
              newDisplay.update(0, vec.display2)
              vec.display3 = withComputedSizes(newDisplay, 4)
              display = vec.display4
            }
            case 5 => {
              newDisplay.update(0, vec.display3)
              vec.display4 = withComputedSizes(newDisplay, 5)
              display = vec.display5
            }
            case 6 => {
              newDisplay.update(0, vec.display4)
              vec.display5 = withComputedSizes(newDisplay, 6)
              display = vec.display6
            }
            case 7 => {
              newDisplay.update(0, vec.display5)
              vec.display6 = withComputedSizes(newDisplay, 7)
            }
          }
          i += 1
        }
      }

      vec.initFocus(0, 0, vec.display0.length, 1, 0)
    } else {

      val newLen = vec.display0.length - n
      val d0 = new Array(newLen).asInstanceOf[Node]
      System.arraycopy(vec.display0, n, d0, 0, newLen)
      vec.display0 = d0
      vec.initFocus(0, 0, newLen, 1, 0)
    }
    vec
  }
}

final class RRBVectorBuilder[ A]
  extends mutable.Builder[A, RRBVector[A]]
    with RRBVectorPointer[A @uncheckedVariance] {

  display0 = new Array(32).asInstanceOf[Node]
  depth = 1

  private var blockIndex = 0
  private var lo = 0
  private var acc: RRBVector[A] = _

  def +=(elem: A): this.type = {
    if (lo >= 32) {
      val newBlockIndex = blockIndex + 32
      gotoNextBlockStartWritable(newBlockIndex, newBlockIndex.^(blockIndex))
      blockIndex = newBlockIndex
      lo = 0
    }
    display0.update(lo, elem.asInstanceOf[Node])
    lo += 1
    this
  }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    if (xs.nonEmpty)
      xs match {
        case thatVec: RRBVector[A] =>
          if (thatVec.length > (1 << 10))
            if (endIndex.!=(0)) {
              acc = this.result().++(xs)
              this.clearCurrent()
            } else if (acc.!=(null))
              acc = acc.++(thatVec)
            else
              acc = thatVec
          else
            super.++=(xs)
        case _ => super.++=(xs)
      }

    this
  }

  def result: RRBVector[A] = {
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

  private[collection] def endIndex = {
    var sz = blockIndex + lo
    if (acc != null)
      sz += acc.endIndex

    sz
  }

  private def currentResult(): RRBVector[A] = {
    val size = blockIndex + lo
    if (size == 0)
      RRBVector.empty
    else {
      val resultVector = new RRBVector[A](size)
      resultVector.initFrom(this)
      resultVector.display0 = copyOf(resultVector.display0, lo, lo)
      val _depth = depth
      if (_depth > 1) {
        resultVector.copyDisplays(_depth, size.-(1))
        resultVector.stabilizeDisplayPath(_depth, size.-(1))
      }
      resultVector.gotoPos(0, size - 1)
      resultVector.initFocus(0, 0, size, _depth, 0)
      resultVector
    }
  }

  private def clearCurrent(): Unit = {
    display0 = new Array(32).asInstanceOf[Node]
    display1 = null
    display2 = null
    display3 = null
    display4 = null
    display5 = null
    display6 = null
    depth = 1
    blockIndex = 0
    lo = 0
  }
}

class RRBVectorIterator[ +A](startIndex: Int,
                            override private[immutable] val endIndex: Int)
  extends AbstractIterator[A]
    with Iterator[A]
    with RRBVectorPointer[A @uncheckedVariance] {
  private var blockIndex: Int = _
  private var lo: Int = _
  private var endLo: Int = _
  private var _hasNext: Boolean = _
  final private[collection] def initIteratorFrom[B >: A](
                                                          that: RRBVectorPointer[B]): Unit = {
    initWithFocusFrom(that)
    _hasNext = startIndex.<(endIndex)
    if (_hasNext) {
      focusOn(startIndex)
      blockIndex = focusStart.+(focus.&(-32))
      lo = focus.&(31)
      if (endIndex.<(focusEnd))
        focusEnd = endIndex
      else
        ()
      endLo = math.min(focusEnd.-(blockIndex), 32)
    } else {
      blockIndex = 0
      lo = 0
      endLo = 1
      display0 = new Array(1).asInstanceOf[Node]
    }
  }
  final def hasNext = _hasNext
  def next(): A = {
    val _lo = lo
    val res: A = display0(_lo).asInstanceOf[A]
    lo = _lo.+(1)
    val _endLo = endLo
    if (_lo.+(1).!=(_endLo))
      res
    else {
      val oldBlockIndex = blockIndex
      val newBlockIndex = oldBlockIndex.+(_endLo)
      blockIndex = newBlockIndex
      lo = 0
      if (newBlockIndex.<(focusEnd)) {
        val _focusStart = focusStart
        val newBlockIndexInFocus = newBlockIndex.-(_focusStart)
        gotoNextBlockStart(newBlockIndexInFocus,
          newBlockIndexInFocus.^(oldBlockIndex.-(_focusStart)))
      } else if (newBlockIndex.<(endIndex)) {
        focusOn(newBlockIndex)
        if (endIndex.<(focusEnd))
          focusEnd = endIndex
        else
          ()
      } else {
        lo = 0
        blockIndex = endIndex
        endLo = 1
        if (_hasNext) {
          _hasNext = false
          return res
        } else
          throw new NoSuchElementException("reached iterator end")
      }
      endLo = math.min(focusEnd.-(newBlockIndex), 32)
      res
    }
  }
  private[collection] def remaining: Int =
    math.max(endIndex.-(blockIndex.+(lo)), 0)
}

class RRBVectorReverseIterator[ +A](startIndex: Int,
                                       final override private[immutable] val endIndex: Int)
  extends AbstractIterator[A]
    with Iterator[A]
    with RRBVectorPointer[A @uncheckedVariance] {

  private var lastIndexOfBlock: Int = _
  private var lo: Int = _
  private var endLo: Int = _
  private var _hasNext: Boolean = startIndex < endIndex

  final private[collection] def initIteratorFrom[ B >: A](that: RRBVectorPointer[B]): Unit = {
    initWithFocusFrom(that)
    _hasNext = startIndex < endIndex
    if (_hasNext) {
      val idx = endIndex - 1
      focusOn(idx)
      lastIndexOfBlock = idx
      lo = idx.-(focusStart).&(31)
      endLo = math.max(startIndex.-(focusStart).-(lastIndexOfBlock), 0)
    } else {
      lastIndexOfBlock = 0
      lo = 0
      endLo = 0
      display0 = new Array(1).asInstanceOf[Node]
    }
  }

  final def hasNext: Boolean = _hasNext

  def next(): A = {
    if (_hasNext) {
      val res = display0(lo).asInstanceOf[A]
      lo -= 1
      if (lo >= endLo)
        res
      else {
        val newBlockIndex = lastIndexOfBlock - 32
        if (focusStart.<=(newBlockIndex)) {
          val _focusStart = focusStart
          val newBlockIndexInFocus = newBlockIndex.-(_focusStart)
          gotoPrevBlockStart(
            newBlockIndexInFocus,
            newBlockIndexInFocus.^(lastIndexOfBlock.-(_focusStart)))
          lastIndexOfBlock = newBlockIndex
          lo = 31
          endLo = math.max(startIndex.-(focusStart).-(focus), 0)
          res
        } else if (startIndex.<(focusStart)) {
          val newIndex = focusStart.-(1)
          focusOn(newIndex)
          lastIndexOfBlock = newIndex
          lo = newIndex.-(focusStart).&(31)
          endLo = math.max(startIndex.-(focusStart).-(lastIndexOfBlock), 0)
          res
        } else {
          _hasNext = false
          res
        }
      }
    } else
      throw new NoSuchElementException("reached iterator end")
  }

}

private[immutable] trait RRBVectorPointer[ +A] {

  type Node = Array[AnyRef]
  //type Leaf[ B >: A] = Array[B]
  val Node: Array.type = Array

  /*Displays*/

  final private[immutable] var display0: Node = _
  final private[immutable] var display1: Node = _
  final private[immutable] var display2: Node = _
  final private[immutable] var display3: Node = _
  final private[immutable] var display4: Node = _
  final private[immutable] var display5: Node = _
  final private[immutable] var display6: Node = _
  final private[immutable] var display7: Node = _

  final private[immutable] var depth: Int = _
  final private[immutable] var focusStart: Int = 0
  final private[immutable] var focusEnd: Int = 0
  final private[immutable] var focusDepth: Int = 0
  final private[immutable] var focus: Int = 0
  final private[immutable] var focusRelax: Int = 0

  private[immutable] def endIndex: Int

  final private[immutable] def initWithFocusFrom[ B >: A](that: RRBVectorPointer[B]): scala.Unit = {
    initFocus(that.focus,
      that.focusStart,
      that.focusEnd,
      that.focusDepth,
      that.focusRelax)
    initFrom(that)
  }

  final private[immutable] def initFocus(focus: Int,
                                         focusStart: Int,
                                         focusEnd: Int,
                                         focusDepth: Int,
                                         focusRelax: Int): scala.Unit = {
    this.focus = focus
    this.focusStart = focusStart
    this.focusEnd = focusEnd
    this.focusDepth = focusDepth
    this.focusRelax = focusRelax
  }

  final private[immutable] def initFrom[ B >: A](that: RRBVectorPointer[B]): scala.Unit = {

    depth = that.depth
    that.depth match {
      case 0 => ()

      case 1 =>
        this.display0 = that.display0.asInstanceOf[Node]

      case 2 =>
        this.display0 = that.display0.asInstanceOf[Node]
        this.display1 = that.display1

      case 3 =>
        this.display0 = that.display0.asInstanceOf[Node]
        this.display1 = that.display1
        this.display2 = that.display2

      case 4 =>
        this.display0 = that.display0.asInstanceOf[Node]
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3

      case 5 =>
        this.display0 = that.display0.asInstanceOf[Node]
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4

      case 6 =>
        this.display0 = that.display0.asInstanceOf[Node]
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4
        this.display5 = that.display5

      case 7 =>
        this.display0 = that.display0.asInstanceOf[Node]
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4
        this.display5 = that.display5
        this.display6 = that.display6

      case _ => throw new IllegalStateException()
    }
  }

  final private[immutable] def initFromRoot(root: Node,
                                            depth: Int): Unit = {
    depth match {
      case 1 => display0 = root.asInstanceOf[Node]
      case 2 => display1 = root
      case 3 => display2 = root
      case 4 => display3 = root
      case 5 => display4 = root
      case 6 => display5 = root
      case 7 => display6 = root
    }
    this.depth = depth
    focusEnd = focusStart
    focusOn(0)
  }


  final private[immutable] def initSingleton[ B >: A](elem: B): scala.Unit = {
    initFocus(0, 0, 1, 1, 0)
    val d0 = new Array(1).asInstanceOf[Node]
    d0.update(0, elem.asInstanceOf[Node])
    display0 = d0
    depth = 1
  }

  final private[immutable] def root(): Node = depth match {
    case 0 => null
    case 1 => display0.asInstanceOf[Node]
    case 2 => display1
    case 3 => display2
    case 4 => display3
    case 5 => display4
    case 6 => display5
    case 7 => display6
    case _ => throw new IllegalStateException()
  }

  final private[immutable] def focusOn(index: Int): Unit = {
    if (focusStart <= index && (index < focusEnd)) {
      val indexInFocus = index - focusStart
      val xor = indexInFocus ^ focus
      if (xor >= 32)
        gotoPos(indexInFocus, xor)
      focus = indexInFocus
    } else
      gotoPosFromRoot(index)
  }

  final private[immutable] def getElementFromRoot(index: Int): A = {

    var indexInSubTree = index
    var currentDepth = depth

    var display: Node = currentDepth match {
      case 2 => display1
      case 3 => display2
      case 4 => display3
      case 5 => display4
      case 6 => display5
      case 7 => display6
    }

    var sizes = display(display.length - 1).asInstanceOf[Array[Int]]

    do {
      val sizesIdx = getIndexInSizes(sizes, indexInSubTree)
      if (sizesIdx != 0)
        indexInSubTree -= sizes(sizesIdx - 1)

      display = display(sizesIdx).asInstanceOf[Node]
      if (currentDepth > 2)
        sizes = display(display.length - 1).asInstanceOf[Array[Int]]
      else
        sizes = null
      currentDepth -= 1
    } while (sizes != null)

    currentDepth match {
      case 1 => getElem0(display.asInstanceOf[Node], indexInSubTree)
      case 2 => getElem1(display, indexInSubTree)
      case 3 => getElem2(display, indexInSubTree)
      case 4 => getElem3(display, indexInSubTree)
      case 5 => getElem4(display, indexInSubTree)
      case 6 => getElem5(display, indexInSubTree)
      case 7 => getElem6(display, indexInSubTree)
      case _ => throw new IllegalStateException()
    }
  }

  final private def getIndexInSizes(sizes: Array[Int],
                                    indexInSubTree: Int): Int = {

    if (indexInSubTree == 0)
      return 0

    var is = 0

    while (sizes(is) <= indexInSubTree) is += 1
    is
  }

  final private[immutable] def gotoPosFromRoot(index: Int): Unit = {

    var _startIndex: Int = 0
    var _endIndex: Int = endIndex
    var currentDepth: Int = depth
    var _focusRelax: Int = 0
    var continue: Boolean = currentDepth.>(1)

    if (continue) {
      var display: Node = currentDepth match {
        case 2 => display1
        case 3 => display2
        case 4 => display3
        case 5 => display4
        case 6 => display5
        case 7 => display6
        case _ => throw new IllegalStateException()
      }
      do {
        val sizes = display(display.length - 1)
          .asInstanceOf[Array[Int]]
        if (sizes.==(null))
          continue = false
        else {
          val is = getIndexInSizes(sizes, index - _startIndex)
          display = display(is).asInstanceOf[Node]
          currentDepth match {
            case 2 =>
              display0 = display.asInstanceOf[Node]
              continue = false
            case 3 => display1 = display
            case 4 => display2 = display
            case 5 => display3 = display
            case 6 => display4 = display
            case 7 => display5 = display
          }
          if (is < sizes.length - 1)
            _endIndex = _startIndex + sizes(is)

          if (is != 0)
            _startIndex += sizes(is - 1)

          currentDepth -= 1
          _focusRelax |= (is << (5 * currentDepth))
        }
      } while (continue)
    }
    val indexInFocus = index - _startIndex
    gotoPos(indexInFocus, 1 << (5 * (currentDepth - 1)))
    initFocus(indexInFocus, _startIndex, _endIndex, currentDepth, _focusRelax)
  }

  final private[immutable] def setupNewBlockInNextBranch(xor: Int,
                                                         transient: Boolean): Unit = {
    if (xor < (1<<10)) {
      if (depth == 1) {
        depth = 2
        val newRoot = new Node(3)
        newRoot.update(0, display0)
        display1 = newRoot
      } else {
        val newRoot = copyAndIncRightRoot(display1, transient, 1)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          withRecomputedSizes(newRoot, 2, oldTransientBranch)
          newRoot.update(oldTransientBranch, display0)
        }
        display1 = newRoot
      }
      display0 = new Array(1).asInstanceOf[Node]

    } else if (xor.<(32768)) {
      if (transient)
        normalize(2)
      else
        ()
      if (depth.==(2)) {
        depth = 3
        display2 = makeNewRoot0(display1)
      } else {
        val newRoot = copyAndIncRightRoot(display2, transient, 2)
        if (transient) {
          val oldTransientBranch = newRoot.length.-(3)
          withRecomputedSizes(newRoot, 3, oldTransientBranch)
          newRoot.update(oldTransientBranch, display1)
        } else
          ()
        display2 = newRoot
      }
      display0 = new Array(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
    } else if (xor.<(1048576)) {
      if (transient)
        normalize(3)
      else
        ()
      if (depth.==(3)) {
        depth = 4
        display3 = makeNewRoot0(display2)
      } else {
        val newRoot = copyAndIncRightRoot(display3, transient, 3)
        if (transient) {
          val oldTransientBranch = newRoot.length.-(3)
          withRecomputedSizes(newRoot, 4, oldTransientBranch)
          newRoot.update(oldTransientBranch, display2)
        } else
          ()
        display3 = newRoot
      }
      display0 = new Array(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
    } else if (xor.<(33554432)) {
      if (transient)
        normalize(4)
      else
        ()
      if (depth.==(4)) {
        depth = 5
        display4 = makeNewRoot0(display3)
      } else {
        val newRoot = copyAndIncRightRoot(display4, transient, 4)
        if (transient) {
          val oldTransientBranch = newRoot.length.-(3)
          withRecomputedSizes(newRoot, 5, oldTransientBranch)
          newRoot.update(oldTransientBranch, display3)
        } else
          ()
        display4 = newRoot
      }
      display0 = new Array(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
    } else if (xor.<(1073741824)) {
      if (transient)
        normalize(5)
      else
        ()
      if (depth.==(5)) {
        depth = 6
        display5 = makeNewRoot0(display4)
      } else {
        val newRoot = copyAndIncRightRoot(display5, transient, 5)
        if (transient) {
          val oldTransientBranch = newRoot.length.-(3)
          withRecomputedSizes(newRoot, 6, oldTransientBranch)
          newRoot.update(oldTransientBranch, display4)
        } else
          ()
        display5 = newRoot
      }
      display0 = new Array(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
      display4 = _emptyTransientBlock
    } else if (xor.<(8)) {
      if (transient)
        normalize(6)
      else
        ()
      if (depth.==(6)) {
        depth = 7
        display6 = makeNewRoot0(display5)
      } else {
        val newRoot = copyAndIncRightRoot(display6, transient, 6)
        if (transient) {
          val oldTransientBranch = newRoot.length.-(3)
          withRecomputedSizes(newRoot, 7, oldTransientBranch)
          newRoot.update(oldTransientBranch, display5)
        } else
          ()
        display6 = newRoot
      }
      display0 = new Array(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
      display4 = _emptyTransientBlock
      display5 = _emptyTransientBlock
    } else
      throw new IllegalArgumentException()
  }

  final private[immutable] def setupNewBlockInInitBranch(insertionDepth: Int,
                                                         transient: Boolean): Unit = insertionDepth match {
    case 2 =>
      if (transient)
        normalize(1)

      if (depth == 1) {
        depth = 2
        val sizes = new Array[Int](2)
        sizes.update(1, display0.length)
        val newRoot = new Node(3)
        newRoot.update(1, display0)
        newRoot.update(2, sizes)
        display1 = newRoot

      } else {
        val newRoot = copyAndIncLeftRoot(display1, transient, 1)
        if (transient) {
          withRecomputedSizes(newRoot, 2, 1)
          newRoot.update(1, display0)
        }
        display1 = newRoot
      }
      display0 = new Node(1).asInstanceOf[Node]
    case 3 =>
      if (transient)
        normalize(2)

      if (depth == 2) {
        depth = 3
        display2 = makeNewRoot1(display1, 3)
      } else {
        val newRoot = copyAndIncLeftRoot(display2, transient, 2)
        if (transient) {
          withRecomputedSizes(newRoot, 3, 1)
          newRoot.update(1, display1)
        }
        display2 = newRoot
      }
      display0 = new Node(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
    case 4 =>
      if (transient)
        normalize(3)

      if (depth == 3) {
        depth = 4
        display3 = makeNewRoot1(display2, 4)
      } else {
        val newRoot = copyAndIncLeftRoot(display3, transient, 3)
        if (transient) {
          withRecomputedSizes(newRoot, 4, 1)
          newRoot.update(1, display2)
        }
        display3 = newRoot
      }
      display0 = new Node(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
    case 5 =>
      if (transient)
        normalize(4)

      if (depth == 4) {
        depth = 5
        display4 = makeNewRoot1(display3, 5)
      } else {
        val newRoot = copyAndIncLeftRoot(display4, transient, 4)
        if (transient) {
          withRecomputedSizes(newRoot, 5, 1)
          newRoot.update(1, display3)
        }
        display4 = newRoot
      }
      display0 = new Node(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
    case 6 =>
      if (transient)
        normalize(5)

      if (depth == 5) {
        depth = 6
        display5 = makeNewRoot1(display4, 6)
      } else {
        val newRoot = copyAndIncLeftRoot(display5, transient, 5)
        if (transient) {
          withRecomputedSizes(newRoot, 6, 1)
          newRoot.update(1, display4)
        }
        display5 = newRoot
      }
      display0 = new Node(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
      display4 = _emptyTransientBlock
    case 7 =>
      if (transient)
        normalize(6)

      if (depth == 6) {
        depth = 7
        display6 = makeNewRoot1(display5, 7)
      } else {
        val newRoot = copyAndIncLeftRoot(display6, transient, 6)
        if (transient) {
          withRecomputedSizes(newRoot, 7, 1)
          newRoot.update(1, display5)
        }
        display6 = newRoot
      }
      display0 = new Node(1).asInstanceOf[Node]
      val _emptyTransientBlock = RRBVector.emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
      display4 = _emptyTransientBlock
      display5 = _emptyTransientBlock
    case _ => throw new IllegalStateException()
  }

  final private[immutable] def gotoPos(index: Int, xor: Int): Unit = {
    if (xor < (1 << 5))
      ()
    else if (xor < (1 << 10))
      display0 = display1(index >> 5 & 31)
        .asInstanceOf[Node]
    else if (xor < (1 << 15)) {
      display1 = display2(index >> 10 & 31)
        .asInstanceOf[Node]
      display0 = display1(index >> 5 & 31)
        .asInstanceOf[Node]
    } else if (xor < (1 << 20)) {
      display2 = display3(index >> 15 & 31)
        .asInstanceOf[Node]
      display1 = display2(index >> 10 & 31)
        .asInstanceOf[Node]
      display0 = display1(index >> 5 & 31)
        .asInstanceOf[Node]
    } else if (xor < (1 << 25)) {
      display3 = display4(index >> 20 & 31)
        .asInstanceOf[Node]
      display2 = display3(index >> 15 & 31)
        .asInstanceOf[Node]
      display1 = display2(index >> 10 & 31)
        .asInstanceOf[Node]
      display0 = display1(index >> 5 & 31)
        .asInstanceOf[Node]
    } else if (xor < (1 << 30)) {
      display4 = display5(index >> 25 & 31)
        .asInstanceOf[Node]
      display3 = display4(index >> 20 & 31)
        .asInstanceOf[Node]
      display2 = display3(index >> 15 & 31)
        .asInstanceOf[Node]
      display1 = display2(index >> 10 & 31)
        .asInstanceOf[Node]
      display0 = display1(index >> 5 & 31)
        .asInstanceOf[Node]
    } else if (xor < (1 << 35)) {
      display5 = display6(index >> 30 & 31)
        .asInstanceOf[Node]
      display4 = display5(index >> 25 & 31)
        .asInstanceOf[Node]
      display3 = display4(index >> 20 & 31)
        .asInstanceOf[Node]
      display2 = display3(index >> 15 & 31)
        .asInstanceOf[Node]
      display1 = display2(index >> 10 & 31)
        .asInstanceOf[Node]
      display0 = display1(index >> 5 & 31)
        .asInstanceOf[Node]
    } else
      throw new IllegalArgumentException()
  }

  final private[immutable] def gotoNextBlockStart(index: Int,
                                                  xor: Int): Unit = {
    var idx = 0
    if (xor >= (1 << 10)) {
      if (xor >= (1 << 15)) {
        if (xor >= (1 << 20)) {
          if (xor >= (1 << 25)) {
            if (xor >= (1 << 30)) {
              if (xor >= (1 << 35))
                throw new IllegalArgumentException()
              else
                display5 = display6(index >> 30 & 31)
                  .asInstanceOf[Node]
              idx = 0
            } else
              idx = index >> 25 & 31
            display4 = display5(idx).asInstanceOf[Node]
            idx = 0
          } else
            idx = index >> 20 & 31
          display3 = display4(idx).asInstanceOf[Node]
          idx = 0
        } else
          idx = index >> 15 & 31
        display2 = display3(idx).asInstanceOf[Node]
        idx = 0
      } else
        idx = index >> 10 & 31
      display1 = display2(idx).asInstanceOf[Node]
      idx = 0
    } else
      idx = index >> 5 & 31
    display0 = display1(idx).asInstanceOf[Node]
  }

  final private[immutable] def gotoPrevBlockStart(index: Int,
                                                  xor: Int): Unit = {
    var idx = 31
    if (xor >= (1 << 10)) {
      if (xor >= (1 << 15)) {
        if (xor >= (1 << 20)) {
          if (xor >= (1 << 25)) {
            if (xor >= (1 << 30)) {
              if (xor >= (1 << 35))
                throw new IllegalArgumentException()
              else
                display5 = display6(index >> 30 & 31)
                  .asInstanceOf[Node]
              idx = 31
            } else
              idx = index >> 25 & 31
            display4 = display5(idx).asInstanceOf[Node]
            idx = 31
          } else
            idx = index >> 20 & 31
          display3 = display4(idx).asInstanceOf[Node]
          idx = 31
        } else
          idx = index >> 15 & 31
        display2 = display3(idx).asInstanceOf[Node]
        idx = 31
      } else
        idx = index >> 10 & 31
      display1 = display2(idx).asInstanceOf[Node]
      idx = 31
    } else
      idx = index >> 5 & 31
    display0 = display1(idx).asInstanceOf[Node]
  }

  final private[immutable] def gotoNextBlockStartWritable(index: Int,
                                                          xor: Int): Unit = {
    if (xor < (1 << 10)) {
      if (depth == 1) {
        display1 = new Node(33)
        display1.update(0, display0)
        depth += 1
      }
      display0 = new Node(32).asInstanceOf[Node]
      display1.update(index >> 5 & 31, display0)
    } else if (xor < (1 << 15)) {
      if (depth == 2) {
        display2 = new Node(33)
        display2.update(0, display1)
        depth += 1
      }
      display0 = new Node(32).asInstanceOf[Node]
      display1 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
    } else if (xor < (1 << 20)) {
      if (depth == 3) {
        display3 = new Node(33)
        display3.update(0, display2)
        depth += 1
      }
      display0 = new Node(32).asInstanceOf[Node]
      display1 = new Node(33)
      display2 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
      display3.update(index >> 15 & 31, display2)
    } else if (xor < (1 << 25)) {
      if (depth.==(4)) {
        display4 = new Node(33)
        display4.update(0, display3)
        depth += 1
      }
      display0 = new Node(32).asInstanceOf[Node]
      display1 = new Node(33)
      display2 = new Node(33)
      display3 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
      display3.update(index >> 15 & 31, display2)
      display4.update(index >> 20 & 31, display3)
    } else if (xor < (1 << 30)) {
      if (depth == 5) {
        display5 = new Node(33)
        display5.update(0, display4)
        depth += 1
      }
      display0 = new Node(32).asInstanceOf[Node]
      display1 = new Node(33)
      display2 = new Node(33)
      display3 = new Node(33)
      display4 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
      display3.update(index >> 15 & 31, display2)
      display4.update(index >> 20 & 31, display3)
      display5.update(index >> 25 & 31, display4)
    } else if (xor < (1 << 35)) {
      if (depth.==(6)) {
        display6 = new Node(33)
        display6.update(0, display5)
        depth += 1
      }
      display0 = new Node(32).asInstanceOf[Node]
      display1 = new Node(33)
      display2 = new Node(33)
      display3 = new Node(33)
      display4 = new Node(33)
      display5 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
      display3.update(index >> 15 & 31, display2)
      display4.update(index >> 20 & 31, display3)
      display5.update(index >> 25 & 31, display4)
      display6.update(index >> 30 & 31, display5)
    } else
      throw new IllegalArgumentException()
  }


  final private[immutable] def normalize(_depth: Int): Unit = {

    val _focusDepth = focusDepth
    val stabilizationIndex = focus.|(focusRelax)
    copyDisplaysAndStabilizeDisplayPath(_focusDepth, stabilizationIndex)

    var currentLevel = _focusDepth
    if (currentLevel.<(_depth)) {
      var display = currentLevel match {
        case 1 => display1
        case 2 => display2
        case 3 => display3
        case 4 => display4
        case 5 => display5
        case 6 => display6
      }
      do {
        val newDisplay = copyOf(display)
        val idx = stabilizationIndex >> (5 * currentLevel) & 31
        currentLevel match {
          case 1 =>
            newDisplay.update(idx, display0)
            display1 = withRecomputedSizes(newDisplay, 2, idx)
            display = display2

          case 2 =>
            newDisplay.update(idx, display1)
            display2 = withRecomputedSizes(newDisplay, 3, idx)
            display = display3

          case 3 =>
            newDisplay.update(idx, display2)
            display3 = withRecomputedSizes(newDisplay, 4, idx)
            display = display4

          case 4 =>
            newDisplay.update(idx, display3)
            display4 = withRecomputedSizes(newDisplay, 5, idx)
            display = display5

          case 5 =>
            newDisplay.update(idx, display4)
            display5 = withRecomputedSizes(newDisplay, 6, idx)
            display = display6

          case 6 =>
            newDisplay.update(idx, display5)
            display6 = withRecomputedSizes(newDisplay, 7, idx)

        }
        currentLevel += 1
      } while (currentLevel < _depth)
    }
  }

  final private[immutable] def copyDisplays(_depth: Int, _focus: Int): Unit = {
    if (2 <= _depth) {
      if (3 <= _depth) {
        if (4 <= _depth) {
          if (5 <= _depth) {
            if (6 <= _depth) {
              if (7 <= _depth) {
                val idx6 = _focus >> 30 & 31 + 1
                display6 = copyOf(display6, idx6, idx6.+(1))
              } else
                ()
              val idx5 = _focus >> 25 & 31 + 1
              display5 = copyOf(display5, idx5, idx5.+(1))
            } else
              ()
            val idx4 = _focus >> 20 & 31 + 1
            display4 = copyOf(display4, idx4, idx4.+(1))
          } else
            ()
          val idx3 = _focus >> 15 & 31 + 1
          display3 = copyOf(display3, idx3, idx3.+(1))
        } else
          ()
        val idx2 = _focus >> 10 & 31 + 1
        display2 = copyOf(display2, idx2, idx2.+(1))
      } else
        ()
      val idx1 = _focus >> 5 & 31 + 1
      display1 = copyOf(display1, idx1, idx1.+(1))
    } else
      ()
  }

  final private[immutable] def copyDisplaysAndNullFocusedBranch( _depth: Int,
                                                                 _focus: Int): Unit = _depth match {
    case 2 => display1 = copyOfAndNull(display1, _focus >> 5 & 31)
    case 3 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)

    case 4 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)

    case 5 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)
      display4 = copyOfAndNull(display4, _focus >> 20 & 31)

    case 6 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)
      display4 = copyOfAndNull(display4, _focus >> 20 & 31)
      display5 = copyOfAndNull(display5, _focus >> 25 & 31)

    case 7 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)
      display4 = copyOfAndNull(display4, _focus >> 20 & 31)
      display5 = copyOfAndNull(display5, _focus >> 25 & 31)
      display6 = copyOfAndNull(display6, _focus >> 30 & 31)

  }

  final private[immutable] def copyDisplaysAndStabilizeDisplayPath( _depth: Int,
                                                                    _focus: Int): Unit = _depth match {
    case 1 => ()
    case 2 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus.>>(5).&(31), display0)
      display1 = d1

    case 3 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus.>>(5).&(31), display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus.>>(10).&(31), d1)
      display2 = d2

    case 4 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus.>>(5).&(31), display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus.>>(10).&(31), d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus.>>(15).&(31), d2)
      display3 = d3

    case 5 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus.>>(5).&(31), display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus.>>(10).&(31), d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus.>>(15).&(31), d2)
      display3 = d3
      val d4: Node = copyOf(display4)
      d4.update(_focus.>>(20).&(31), d3)
      display4 = d4

    case 6 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus.>>(5).&(31), display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus.>>(10).&(31), d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus.>>(15).&(31), d2)
      display3 = d3
      val d4: Node = copyOf(display4)
      d4.update(_focus.>>(20).&(31), d3)
      display4 = d4
      val d5: Node = copyOf(display5)
      d5.update(_focus.>>(25).&(31), d4)
      display5 = d5

    case 7 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus.>>(5).&(31), display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus.>>(10).&(31), d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus.>>(15).&(31), d2)
      display3 = d3
      val d4: Node = copyOf(display4)
      d4.update(_focus.>>(20).&(31), d3)
      display4 = d4
      val d5: Node = copyOf(display5)
      d5.update(_focus.>>(25).&(31), d4)
      display5 = d5
      val d6: Node = copyOf(display6)
      d6.update(_focus.>>(30).&(31), d5)
      display6 = d6

  }

  final private[immutable] def copyDisplaysTop(currentDepth: Int,
                                               _focusRelax: Int): Unit = {
    var _currentDepth = currentDepth
    while (_currentDepth.<(this.depth)) {
      _currentDepth match {
        case 2 => {
          val cutIndex = _focusRelax.>>(5).&(31)
          display1 = copyOf(display1, cutIndex.+(1), cutIndex.+(2))
        }
        case 3 => {
          val cutIndex = _focusRelax.>>(10).&(31)
          display2 = copyOf(display2, cutIndex.+(1), cutIndex.+(2))
        }
        case 4 => {
          val cutIndex = _focusRelax.>>(15).&(31)
          display3 = copyOf(display3, cutIndex.+(1), cutIndex.+(2))
        }
        case 5 => {
          val cutIndex = _focusRelax.>>(20).&(31)
          display4 = copyOf(display4, cutIndex.+(1), cutIndex.+(2))
        }
        case 6 => {
          val cutIndex = _focusRelax.>>(25).&(31)
          display5 = copyOf(display5, cutIndex.+(1), cutIndex.+(2))
        }
        case 7 => {
          val cutIndex = _focusRelax.>>(30).&(31)
          display6 = copyOf(display6, cutIndex.+(1), cutIndex.+(2))
        }
        case _ => throw new IllegalStateException()
      }
      _currentDepth.+=(1)
    }

  }

  final private[immutable] def stabilizeDisplayPath(_depth: Int,
                                                    _focus: Int): Unit = if(1 < _depth){
      val d1 = display1
      d1.update(_focus >> 5 & 31, display0)
      if (2 < _depth) {
        val d2 = display2
        d2.update(_focus >> 10 & 31, d1)
        if (3 < _depth) {
          val d3 = display3
          d3.update(_focus >> 15 & 31, d2)
          if (4 < _depth) {
            val d4 = display4
            d4.update(_focus >> 20 & 31, d3)
            if (5 < _depth) {
              val d5 = display5
              d5.update(_focus >> 25 & 31, d4)
              if (_depth == 7)
                display6.update(_focus >> 30 & 31, d5)
            }
          }
        }
      }
    }

  private[immutable] def cleanTopTake(cutIndex: Int): Unit = this.depth match {
      case 2 =>
        if ((cutIndex >> 5) == 0) {
          display1 = null
          this.depth = 1
        } else
          this.depth = 2
      case 3 =>
        if ((cutIndex >> 10) == 0) {
          display2 = null
          if ((cutIndex >> 5) == 0) {
            display1 = null
            this.depth = 1
          } else
            this.depth = 2
        } else
          this.depth = 3
      case 4 =>
        if ((cutIndex >> 15) == 0) {
          display3 = null
          if ((cutIndex >> 10) == 0) {
            display2 = null
            if ((cutIndex >> 5) == 0) {
              display1 = null
              this.depth = 1
            } else
              this.depth = 2
          } else
            this.depth = 3
        } else
          this.depth = 4
      case 5 =>
        if ((cutIndex >> 20) == 0) {
          display4 = null
          if ((cutIndex >> 15) == 0) {
            display3 = null
            if ((cutIndex >> 10) == 0) {
              display2 = null
              if ((cutIndex >> 5) == 0) {
                display1 = null
                this.depth = 1
              } else
                this.depth = 2
            } else
              this.depth = 3
          } else
            this.depth = 4
        } else
          this.depth = 5
      case 6 =>
        if ((cutIndex >> 25) == 0) {
          display5 = null
          if ((cutIndex >> 20) == 0) {
            display4 = null
            if ((cutIndex >> 15) == 0) {
              display3 = null
              if ((cutIndex >> 10) == 0) {
                display2 = null
                if ((cutIndex >> 5) == 0) {
                  display1 = null
                  this.depth = 1
                } else
                  this.depth = 2
              } else
                this.depth = 3
            } else
              this.depth = 4
          } else
            this.depth = 5
        } else
          this.depth = 6
      case 7 =>
        if ((cutIndex >> 30) == 0) {
          display6 = null
          if ((cutIndex >> 25) == 0) {
            display5 = null
            if ((cutIndex >> 20) == 0) {
              display4 = null
              if ((cutIndex >> 15) == 0) {
                display3 = null
                if ((cutIndex >> 10) == 0) {
                  display2 = null
                  if ((cutIndex >> 5) == 0) {
                    display1 = null
                    this.depth = 1
                  } else
                    this.depth = 2
                } else
                  this.depth = 3
              } else
                this.depth = 4
            } else
              this.depth = 5
          } else
            this.depth = 6
        } else
          this.depth = 7
    }

  private[immutable] def cleanTopDrop(cutIndex: Int): Unit = this.depth match {
      case 2 =>
        if ((cutIndex >> 5) == (display1.length - 2)) {
          display1 = null
          this.depth = 1
        } else
          this.depth = 2
      case 3 =>
        if ((cutIndex >> 10) == (display2.length - 2)) {
          display2 = null
          if ((cutIndex >> 5) == (display1.length - 2)) {
            display1 = null
            this.depth = 1
          } else
            this.depth = 2
        } else
          this.depth = 3
      case 4 =>
        if ((cutIndex >> 15) == (display3.length - 2)) {
          display3 = null
          if ((cutIndex >> 10) == (display2.length - 2)) {
            display2 = null
            if ((cutIndex >> 5) == (display1.length - 2)) {
              display1 = null
              this.depth = 1
            } else
              this.depth = 2
          } else
            this.depth = 3
        } else
          this.depth = 4
      case 5 =>
        if ((cutIndex >> 20) == (display4.length - 2)) {
          display4 = null
          if ((cutIndex >> 15) == (display3.length - 2)) {
            display3 = null
            if ((cutIndex >> 10) == (display2.length - 2)) {
              display2 = null
              if ((cutIndex >> 5) == (display1.length - 2)) {
                display1 = null
                this.depth = 1
              } else
                this.depth = 2
            } else
              this.depth = 3
          } else
            this.depth = 4
        } else
          this.depth = 5
      case 6 =>
        if ((cutIndex >> 25) == (display5.length - 2)) {
          display5 = null
          if ((cutIndex >> 20) == (display4.length - 2)) {
            display4 = null
            if ((cutIndex >> 15) == (display3.length - 2)) {
              display3 = null
              if ((cutIndex >> 10) == (display2.length - 2)) {
                display2 = null
                if ((cutIndex >> 5) == (display1.length - 2)) {
                  display1 = null
                  this.depth = 1
                } else
                  this.depth = 2
              } else
                this.depth = 3
            } else
              this.depth = 4
          } else
            this.depth = 5
        } else
          this.depth = 6
      case 7 =>
        if ((cutIndex >> 30) == (display6.length - 2)) {
          display6 = null
          if ((cutIndex >> 25) == (display5.length - 2)) {
            display5 = null
            if ((cutIndex >> 20) == (display4.length - 2)) {
              display4 = null
              if ((cutIndex >> 15) == (display3.length - 2)) {
                display3 = null
                if ((cutIndex >> 10) == (display2.length - 2)) {
                  display2 = null
                  if ((cutIndex >> 5) == (display1.length - 2)) {
                    display1 = null
                    this.depth = 1
                  } else
                    this.depth = 2
                } else
                  this.depth = 3
              } else
                this.depth = 4
            } else
              this.depth = 5
          } else
            this.depth = 6
        } else
          this.depth = 7
    }

  final private[immutable] def copyOf[AnyRef](array: Node) = {
    val len = array.length
    val newArray = new Array(len).asInstanceOf[Node]
    System.arraycopy(array, 0, newArray, 0, len)
    newArray
  }

  final private[immutable] def copyOf[AnyRef](array: Node,
                                              numElements: Int,
                                              newSize: Int) = {
    val newArray = new Array(newSize).asInstanceOf[Node]
    System.arraycopy(array, 0, newArray, 0, numElements)
    newArray
  }

  final private[immutable] def copyOfAndNull(array: Node,
                                             nullIndex: Int) = {
    val len = array.length
    val newArray = new Node(len).asInstanceOf[Node]
    System.arraycopy(array, 0, newArray, 0, len - 1)
    newArray.update(nullIndex, null)
    val sizes = array(len - 1).asInstanceOf[Array[Int]]
    if (sizes != null)
      newArray.update(len - 1, makeTransientSizes(sizes, nullIndex))

    newArray
  }

  final private def makeNewRoot0(node: Node): Node = {
    val newRoot = new Node(3)
    newRoot.update(0, node)
    val dLen = node.length
    val dSizes = node(dLen - 1)

    if (dSizes != null) {
      val newRootSizes = new Array[Int](2)
      val dSize = dSizes.asInstanceOf[Array[Int]](dLen.-(2))
      newRootSizes.update(0, dSize)
      newRootSizes.update(1, dSize)
      newRoot.update(2, newRootSizes)
    }
    newRoot
  }

  final private def makeNewRoot1(node: Node,
                                 currentDepth: Int): Node = {
    val dSize = treeSize(node, currentDepth - 1)
    val newRootSizes = new Array[Int](2)
    newRootSizes.update(1, dSize)
    val newRoot = new Node(3)
    newRoot.update(1, node)
    newRoot.update(2, newRootSizes)
    newRoot
  }

  final private[immutable] def makeTransientSizes(oldSizes: Array[Int],
                                                  transientBranchIndex: Int) = {
    val newSizes = new Array[Int](oldSizes.length)
    var delta = oldSizes(transientBranchIndex)
    if (transientBranchIndex > 0) {
      delta -= oldSizes(transientBranchIndex - 1)
      if (!oldSizes.eq(newSizes).`unary_!`)
        System.arraycopy(oldSizes, 0, newSizes, 0, transientBranchIndex)
    }

    var i = transientBranchIndex
    val len = newSizes.length
    while (i < len) {
      newSizes.update(i, oldSizes(i) - delta)
      i += 1
    }
    newSizes
  }

  final private def copyAndIncRightRoot(node: Node,
                                        transient: Boolean,
                                        currentLevel: Int): Node = {
    val len = node.length
    val newRoot = copyOf(node, len.-(1), len.+(1))
    val oldSizes = node(len.-(1)).asInstanceOf[Array[Int]]
    if (oldSizes != null) {
      val newSizes = new Array[Int](len)
      System.arraycopy(oldSizes, 0, newSizes, 0, len.-(1))
      if (transient)
        newSizes.update(len - 1, 1 << (5 * currentLevel))

      newSizes.update(len - 1, newSizes(len - 2))
      newRoot.update(len, newSizes)
    }
    newRoot
  }

  final private def copyAndIncLeftRoot(node: Node,
                                       transient: Boolean,
                                       currentLevel: Int) = {
    val len = node.length
    val newRoot = new Node(len + 1)
    System.arraycopy(node, 0, newRoot, 1, len - 1)

    val oldSizes = node(len - 1)
    val newSizes = new Array[Int](len)

    if (oldSizes != null)
      if (transient)
        System.arraycopy(oldSizes, 1, newSizes, 2, len - 2)
      else
        System.arraycopy(oldSizes, 0, newSizes, 1, len - 1)
    else {
      val subTreeSize = 1<<(5*currentLevel)
      var acc = 0
      var i = 1
      while (i < (len - 1)) {
        acc += subTreeSize
        newSizes.update(i, acc)
        i += 1
      }
      newSizes.update(i, acc + treeSize(node(node.length - 2).asInstanceOf[Node],
        currentLevel))
    }
    newRoot.update(len, newSizes)
    newRoot
  }

  final private[immutable] def withComputedSizes1(node: Node) = {
    var i = 0
    var acc = 0
    val end = node.length - 1

    if (end > 1) {
      val sizes = new Array[Int](end)
      while (i < end) {
        acc += node(i).asInstanceOf[Node].length
        sizes.update(i, acc)
        i += 1
      }
      if (sizes(end - 2) != (end - 1<<5))
        node.update(end, sizes)
    }
    node
  }

  final private[immutable] def withComputedSizes(node: Node,
                                                 currentDepth: Int) = {
    var i = 0
    var acc = 0
    val end = node.length - 1

    if (end > 1) {
      val sizes = new Array[Int](end)
      while (i < end) {
        acc += treeSize(node(i).asInstanceOf[Node], currentDepth - 1)
        sizes.update(i, acc)
        i += 1
      }
      if(notBalanced(node, sizes, currentDepth, end))
        node.update(end, sizes)
    } else if (end == 1 && currentDepth > 2) {
      val child = node(0).asInstanceOf[Node]
      val childSizes = child(child.length - 1).asInstanceOf[Array[Int]]

      if (childSizes != null)
        if (childSizes.length != 1) {
          val sizes = new Array[Int](1)
          sizes.update(0, childSizes(childSizes.length - 1))
          node.update(end, sizes)
        } else
          node.update(end, childSizes)
    }
    node
  }

  final private def withRecomputedSizes(node: Node,
                                        currentDepth: Int,
                                        branchToUpdate: Int) = {
    val end = node.length - 1
    val oldSizes = node(end).asInstanceOf[Array[Int]]

    if (oldSizes != null) {
      val newSizes = new Array[Int](end)
      val delta = treeSize(node(branchToUpdate).asInstanceOf[Node],
        currentDepth - 1)
      if (branchToUpdate > 0)
        System.arraycopy(oldSizes, 0, newSizes, 0, branchToUpdate)

      var i = branchToUpdate
      while (i.<(end)) {
        newSizes.update(i, oldSizes(i) + delta)
        i += 1
      }
      if (notBalanced(node, newSizes, currentDepth, end))
        node.update(end, newSizes)
    }
    node
  }

  @inline final private def notBalanced(node: Node,
                                        sizes: Array[Int],
                                        currentDepth: Int,
                                        end: Int): Boolean = {
    sizes(end - 2) != (end - (1 << 5*(currentDepth - 1))) || (currentDepth > 2) && {
      val last = node(end.-(1)).asInstanceOf[Node]
      last(last.length.-(1)).!=(null)
    }
  }

  private def treeSize(tree: Node, currentDepth: Int) = {
    def treeSizeRec(node: Node, currentDepth: Int, acc: Int): Int =
      if (currentDepth == 1)
        acc + node.length
      else {
        val treeSizes: Array[Int] = node(node.length - 1).asInstanceOf[Array[Int]]
        if (treeSizes != null)
          acc + treeSizes(treeSizes.length - 1)
        else {
          val len = node.length
          treeSizeRec(node(len - 2).asInstanceOf[Node],
            currentDepth - 1,
            acc + (len - 2 * (1<<(5*(currentDepth - 1))))
          )
        }
      }
    treeSizeRec(tree, currentDepth, 0)
  }

  final private[immutable] def getElem(index: Int, xor: Int): A = {
    if (xor < (1<<5))
      getElem0(display0, index)
    else if (xor < (1<<10))
      getElem1(display1, index)
    else if (xor < (1<<15))
      getElem2(display2, index)
    else if (xor < (1<<20))
      getElem3(display3, index)
    else if (xor < (1<<25))
      getElem4(display4, index)
    else if (xor < (1<<30))
      getElem5(display5, index)
    else if (xor < (1<<35))
      getElem6(display6, index)
    else
      throw new IllegalArgumentException()
  }

  final private def getElem0(block: Node, index: Int): A = block(index & 31).asInstanceOf[A]

  final private def getElem1(block: Node, index: Int): A =
    block(index >> 5 & 31)
      .asInstanceOf[Node](index.&(31))
      .asInstanceOf[A]

  final private def getElem2(block: Node, index: Int): A =
    block(index.>>(10).&(31))
      .asInstanceOf[Node](index.>>(5).&(31))
      .asInstanceOf[Node](index.&(31))
      .asInstanceOf[A]

  final private def getElem3(block: Node, index: Int): A =
    block(index.>>(15).&(31))
      .asInstanceOf[Node](index.>>(10).&(31))
      .asInstanceOf[Node](index.>>(5).&(31))
      .asInstanceOf[Node](index.&(31))
      .asInstanceOf[A]

  final private def getElem4(block: Node, index: Int): A =
    block(index.>>(20).&(31))
      .asInstanceOf[Node](index.>>(15).&(31))
      .asInstanceOf[Node](index.>>(10).&(31))
      .asInstanceOf[Node](index.>>(5).&(31))
      .asInstanceOf[Node](index.&(31))
      .asInstanceOf[A]

  final private def getElem5(block: Node, index: Int): A =
    block(index.>>(25).&(31))
      .asInstanceOf[Node](index.>>(20).&(31))
      .asInstanceOf[Node](index.>>(15).&(31))
      .asInstanceOf[Node](index.>>(10).&(31))
      .asInstanceOf[Node](index.>>(5).&(31))
      .asInstanceOf[Node](index.&(31))
      .asInstanceOf[A]

  final private def getElem6(block: Node, index: Int): A =
    block(index.>>(30).&(31))
      .asInstanceOf[Node](index.>>(25).&(31))
      .asInstanceOf[Node](index.>>(20).&(31))
      .asInstanceOf[Node](index.>>(15).&(31))
      .asInstanceOf[Node](index.>>(10).&(31))
      .asInstanceOf[Node](index.>>(5).&(31))
      .asInstanceOf[Node](index.&(31))
      .asInstanceOf[A]

  private[immutable] def debugToString(): String = {
    val sb = new StringBuilder()
    sb.append("RRBVector (\n")
    sb.append(
      "\t"
        .+("display0")
        .+(" = ")
        .+(display0)
        .+(" ")
        .+(
          if (display0.!=(null))
            display0.mkString("[", ", ", "]")
          else
            "")
        .+("\n"))
    sb.append(
      "\t"
        .+("display1")
        .+(" = ")
        .+(display1)
        .+(" ")
        .+(
          if (display1.!=(null))
            display1.mkString("[", ", ", "]")
          else
            "")
        .+("\n"))
    sb.append(
      "\t"
        .+("display2")
        .+(" = ")
        .+(display2)
        .+(" ")
        .+(
          if (display2.!=(null))
            display2.mkString("[", ", ", "]")
          else
            "")
        .+("\n"))
    sb.append(
      "\t"
        .+("display3")
        .+(" = ")
        .+(display3)
        .+(" ")
        .+(
          if (display3.!=(null))
            display3.mkString("[", ", ", "]")
          else
            "")
        .+("\n"))
    sb.append(
      "\t"
        .+("display4")
        .+(" = ")
        .+(display4)
        .+(" ")
        .+(
          if (display4.!=(null))
            display4.mkString("[", ", ", "]")
          else
            "")
        .+("\n"))
    sb.append(
      "\t"
        .+("display5")
        .+(" = ")
        .+(display5)
        .+(" ")
        .+(
          if (display5.!=(null))
            display5.mkString("[", ", ", "]")
          else
            "")
        .+("\n"))
    sb.append(
      "\t"
        .+("display6")
        .+(" = ")
        .+(display6)
        .+(" ")
        .+(
          if (display6.!=(null))
            display6.mkString("[", ", ", "]")
          else
            "")
        .+("\n"))
    sb.append("\tdepth = ".+(depth).+("\n"))
    sb.append("\tendIndex = ".+(endIndex).+("\n"))
    sb.append("\tfocus = ".+(focus).+("\n"))
    sb.append("\tfocusStart = ".+(focusStart).+("\n"))
    sb.append("\tfocusEnd = ".+(focusEnd).+("\n"))
    sb.append("\tfocusRelax = ".+(focusRelax).+("\n"))
    sb.append(")")
    sb.toString
  }
}
