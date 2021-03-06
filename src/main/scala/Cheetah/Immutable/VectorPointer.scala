package Cheetah.Immutable

import scala.annotation.tailrec
import scala.reflect.ClassTag

private[Immutable] trait VectorPointer[A] {

  type Node = Array[AnyRef]
  type Leaf = Array[A]
  type Size = Array[Int]

  final private[Immutable] var display0: Leaf = _
  final private[Immutable] var display1: Node = _
  final private[Immutable] var display2: Node = _
  final private[Immutable] var display3: Node = _
  final private[Immutable] var display4: Node = _
  final private[Immutable] var display5: Node = _
  final private[Immutable] var display6: Node = _
  final private[Immutable] var display7: Node = _

  final private[Immutable] var depth: Int = 0
  final private[Immutable] var focusStart: Int = 0
  final private[Immutable] var focusEnd: Int = 0
  final private[Immutable] var focusDepth: Int = 0
  final private[Immutable] var focus: Int = 0
  final private[Immutable] var focusRelax: Int = 0

  private[Immutable] def endIndex: Int

  final private[Immutable] def initWithFocusFrom(that: VectorPointer[A]): Unit = {
    initFocus(that.focus,
      that.focusStart,
      that.focusEnd,
      that.focusDepth,
      that.focusRelax)
    initFrom(that)
  }

  final private[Immutable] def initFocus(focus: Int,
                                         focusStart: Int,
                                         focusEnd: Int,
                                         focusDepth: Int,
                                         focusRelax: Int): Unit = {
    this.focus = focus
    this.focusStart = focusStart
    this.focusEnd = focusEnd
    this.focusDepth = focusDepth
    this.focusRelax = focusRelax
  }

  final private[Immutable] def initFrom(that: VectorPointer[A]): Unit = {
    this.depth = that.depth
    that.depth match {
      case 1 =>
        this.display0 = that.display0
        this.display1 = that.display1

      case 2 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2

      case 3 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3

      case 4 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4

      case 5 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4
        this.display5 = that.display5

      case 6 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4
        this.display5 = that.display5
        this.display6 = that.display6

      case 7 =>
        this.display0 = that.display0
        this.display1 = that.display1
        this.display2 = that.display2
        this.display3 = that.display3
        this.display4 = that.display4
        this.display5 = that.display5
        this.display6 = that.display6
        this.display7 = that.display7

      case _ => throw new IllegalStateException()
    }
  }

  private[Immutable] final def initFromRoot(root: Node,
                                            depth: Int): Unit = {
    depth match {
      case 1 => display1 = root
      case 2 => display2 = root
      case 3 => display3 = root
      case 4 => display4 = root
      case 5 => display5 = root
      case 6 => display6 = root
      case 7 => display7 = root
    }

    this.depth = depth
    focusEnd = focusStart
    focusOn(0)
  }

  final private[Immutable] def initSingleton(elem: A)(implicit ct: ClassTag[A]): Unit = {
    initFocus(0, 0, 1, 1, 0)
    val d0: Leaf = new Leaf(1)
    val d1: Node = new Node(3)
    val size: Size = new Size(1)

    d0.update(0, elem)
    size.update(0, d0.length)
    display0 = d0.asInstanceOf[Leaf]

    d1.update(0, display0)
    //d1 = withComputedSizes(d1, 1)
    d1.update(2, size)

    display1 = d1
    depth = 1
  }

  final private[Immutable] def root: Node = {
    depth match {
      case 1 => display1
      case 2 => display2
      case 3 => display3
      case 4 => display4
      case 5 => display5
      case 6 => display6
      case 7 => display7
      case _ => throw new IllegalStateException()
    }
  }

  final private[Immutable] def focusOn(index: Int): Unit = {
    if (focusStart <= index && index < focusEnd) {
      val indexInFocus: Int = index - focusStart
      val xor: Int = indexInFocus ^ focus
      if (xor >= 32)
        gotoPos(indexInFocus, xor)
      focus = indexInFocus
    } else {
      gotoPosFromRoot(index)
    }
  }

  final private[Immutable] def getElementFromRoot(index: Int): A = {

    var indexInSubTree: Int = index
    var currentDepth: Int = depth
    var display: Node = currentDepth match {
      case 1 => display1
      case 2 => display2
      case 3 => display3
      case 4 => display4
      case 5 => display5
      case 6 => display6
      case 7 => display7
      case _ => throw new IllegalStateException()
    }
    var sizes: Size = display(display.length - 1).asInstanceOf[Size]

    do {
      val sizesIdx: Int = getIndexInSizes(sizes, indexInSubTree)
      if (sizesIdx != 0) indexInSubTree -= sizes(sizesIdx - 1)
      if (currentDepth >= 2) {
        display = display(sizesIdx).asInstanceOf[Node]
        sizes = display(display.length - 1).asInstanceOf[Size]
        currentDepth -= 1
      }
    } while (currentDepth > 1)

    currentDepth match {
      case 1 => getElem1(display, indexInSubTree)
      case 2 => getElem2(display, indexInSubTree)
      case 3 => getElem3(display, indexInSubTree)
      case 4 => getElem4(display, indexInSubTree)
      case 5 => getElem5(display, indexInSubTree)
      case 6 => getElem6(display, indexInSubTree)
      case 7 => getElem7(display, indexInSubTree)
      case _ => throw new IllegalStateException()
    }
  }

  @inline final private def getIndexInSizes(sizes: Size,
                                            indexInSubTree: Int): Int = {
    if (indexInSubTree == 0) return 0

    var is = 0
    while (sizes(is) <= indexInSubTree) is += 1
    is
  }

  final private[Immutable] def gotoPosFromRoot(index: Int): Unit = {
    //TODO Check for correctness: Seems fine
    var _startIndex: Int = 0
    var _endIndex: Int = endIndex
    var currentDepth: Int = depth
    var _focusRelax: Int = 0
    var continue: Boolean = currentDepth > 0

    if (continue) {
      var display: Node = currentDepth match {
        case 1 => display1
        case 2 => display2
        case 3 => display3
        case 4 => display4
        case 5 => display5
        case 6 => display6
        case 7 => display7
        case _ => throw new IllegalStateException(currentDepth.toString)
      }

      do {
        val sizes = display(display.length - 1).asInstanceOf[Size]
        if (sizes == null)
          continue = false
        else {
          val is: Int = getIndexInSizes(sizes, index - _startIndex)
          display = display(is).asInstanceOf[Node]
          currentDepth match {
            case 1 =>
              display0 = display.asInstanceOf[Leaf]
              continue = false
            case 2 => display1 = display
            case 3 => display2 = display
            case 4 => display3 = display
            case 5 => display4 = display
            case 6 => display5 = display
            case 7 => display6 = display
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
    val indexInFocus: Int = index - _startIndex
    gotoPos(indexInFocus, 1 << (5 * currentDepth))
    initFocus(indexInFocus, _startIndex, _endIndex, currentDepth, _focusRelax)
  }

  final private[Immutable] def setupNewBlockInNextBranch(xor: Int,
                                                         transient: Boolean)(implicit ct: ClassTag[A]): Unit = {
    if (xor < (1 << 10)) {

      if (transient) normalize(1)
      val newRoot: Node = copyAndIncRightRoot(display1, transient, 1)
      if (transient) {
        val oldTransientBranch: Int = newRoot.length - 3
        withRecomputedSizes(newRoot, 1, oldTransientBranch)
        newRoot.update(oldTransientBranch, display0)
      }
      display1 = newRoot
      display0 = new Leaf(1)

    } else if (xor < (1 << 15)) {

      if (transient) normalize(2)
      if (depth == 1) {
        depth = 2
        display2 = makeNewRoot0(display1)
      } else {
        val newRoot = copyAndIncRightRoot(display2, transient, 2)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          withRecomputedSizes(newRoot, 2, oldTransientBranch)
          newRoot.update(oldTransientBranch, display1)
        }
        display2 = newRoot
      }
      display0 = new Leaf(1)
      lazy val _emptyTransientBlock = new Node(2) //emptyTransientBlock
      display1 = _emptyTransientBlock

    } else if (xor < (1 << 20)) {

      if (transient) normalize(3)
      if (depth == 2) {
        depth = 3
        display3 = makeNewRoot0(display2)
      } else {
        val newRoot = copyAndIncRightRoot(display3, transient, 3)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          withRecomputedSizes(newRoot, 3, oldTransientBranch)
          newRoot.update(oldTransientBranch, display2)
        }
        display3 = newRoot
      }
      display0 = new Leaf(1)
      val _emptyTransientBlock = new Node(2) //emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock

    } else if (xor < (1 << 25)) {

      if (transient) normalize(4)
      if (depth == 3) {
        depth = 4
        display4 = makeNewRoot0(display3)
      } else {
        val newRoot = copyAndIncRightRoot(display4, transient, 4)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          withRecomputedSizes(newRoot, 4, oldTransientBranch)
          newRoot.update(oldTransientBranch, display3)
        }
        display4 = newRoot
      }
      display0 = new Leaf(1)
      val _emptyTransientBlock = new Node(2) //emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock

    } else if (xor < (1 << 30)) {

      if (transient) normalize(5)
      if (depth == 4) {
        depth = 5
        display5 = makeNewRoot0(display4)
      } else {
        val newRoot = copyAndIncRightRoot(display5, transient, 5)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          withRecomputedSizes(newRoot, 5, oldTransientBranch)
          newRoot.update(oldTransientBranch, display4)
        }
        display5 = newRoot
      }
      display0 = new Leaf(1)
      val _emptyTransientBlock = new Node(2) //emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
      display4 = _emptyTransientBlock

    } else if (xor < (1 << 35)) {

      if (transient) normalize(6)
      if (depth == 5) {
        depth = 6
        display6 = makeNewRoot0(display5)
      } else {
        val newRoot = copyAndIncRightRoot(display6, transient, 6)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          withRecomputedSizes(newRoot, 6, oldTransientBranch)
          newRoot.update(oldTransientBranch, display5)
        }
        display6 = newRoot
      }
      display0 = new Leaf(1)
      val _emptyTransientBlock = new Node(2) //emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
      display4 = _emptyTransientBlock
      display5 = _emptyTransientBlock

    } else if (xor < (1 << 40)) {

      if (transient) normalize(7)
      if (depth == 6) {
        depth = 7
        display7 = makeNewRoot0(display6)
      } else {
        val newRoot = copyAndIncRightRoot(display7, transient, 7)
        if (transient) {
          val oldTransientBranch = newRoot.length - 3
          withRecomputedSizes(newRoot, 7, oldTransientBranch)
          newRoot.update(oldTransientBranch, display6)
        }
        display7 = newRoot
      }
      display0 = new Leaf(1)
      val _emptyTransientBlock = new Node(2) //emptyTransientBlock
      display1 = _emptyTransientBlock
      display2 = _emptyTransientBlock
      display3 = _emptyTransientBlock
      display4 = _emptyTransientBlock
      display5 = _emptyTransientBlock
      display6 = _emptyTransientBlock

    } else
      throw new IllegalArgumentException()
  }

  final private[Immutable] def setupNewBlockInInitBranch(insertionDepth: Int,
                                                         transient: Boolean)(implicit ct: ClassTag[A]): Unit = {
    insertionDepth match {
      case 1 =>
        if (transient) normalize(1)

        val newRoot: Node = copyAndIncLeftRoot(display1, transient, 1)
        if (transient) {
          withRecomputedSizes(newRoot, 1, 1)
          newRoot.update(1, display0)
        }
        display1 = newRoot
        display0 = new Leaf(1)

      case 2 =>
        if (transient) normalize(2)

        if (depth == 1) {
          depth = 2
          display2 = makeNewRoot1(display1, 2)
        } else {
          val newRoot = copyAndIncLeftRoot(display2, transient, 2)
          if (transient) {
            withRecomputedSizes(newRoot, 2, 1)
            newRoot.update(1, display1)
          }
          display2 = newRoot
        }
        display0 = new Leaf(1)
        val _emptyTransientBlock = new Node(2) //emptyTransientBlock
        display1 = _emptyTransientBlock

      case 3 =>
        if (transient) normalize(3)

        if (depth == 2) {
          depth = 3
          display3 = makeNewRoot1(display2, 3)
        } else {
          val newRoot = copyAndIncLeftRoot(display3, transient, 3)
          if (transient) {
            withRecomputedSizes(newRoot, 3, 1)
            newRoot.update(1, display2)
          }
          display3 = newRoot
        }
        display0 = new Leaf(1)
        val _emptyTransientBlock = new Node(2) //emptyTransientBlock
        display1 = _emptyTransientBlock
        display2 = _emptyTransientBlock

      case 4 =>
        if (transient) normalize(4)

        if (depth == 3) {
          depth = 4
          display4 = makeNewRoot1(display3, 4)
        } else {
          val newRoot = copyAndIncLeftRoot(display4, transient, 4)
          if (transient) {
            withRecomputedSizes(newRoot, 4, 1)
            newRoot.update(1, display3)
          }
          display4 = newRoot
        }
        display0 = new Leaf(1)
        val _emptyTransientBlock = new Node(2) //emptyTransientBlock
        display1 = _emptyTransientBlock
        display2 = _emptyTransientBlock
        display3 = _emptyTransientBlock

      case 5 =>
        if (transient) normalize(5)

        if (depth == 4) {
          depth = 5
          display5 = makeNewRoot1(display4, 5)
        } else {
          val newRoot = copyAndIncLeftRoot(display5, transient, 5)
          if (transient) {
            withRecomputedSizes(newRoot, 5, 1)
            newRoot.update(1, display4)
          }
          display5 = newRoot
        }
        display0 = new Leaf(1)
        val _emptyTransientBlock = new Node(2) //emptyTransientBlock
        display1 = _emptyTransientBlock
        display2 = _emptyTransientBlock
        display3 = _emptyTransientBlock
        display4 = _emptyTransientBlock

      case 6 =>
        if (transient) normalize(6)

        if (depth == 5) {
          depth = 6
          display6 = makeNewRoot1(display5, 6)
        } else {
          val newRoot = copyAndIncLeftRoot(display6, transient, 6)
          if (transient) {
            withRecomputedSizes(newRoot, 6, 1)
            newRoot.update(1, display5)
          }
          display6 = newRoot
        }
        display0 = new Leaf(1)
        val _emptyTransientBlock = new Node(2) //emptyTransientBlock
        display1 = _emptyTransientBlock
        display2 = _emptyTransientBlock
        display3 = _emptyTransientBlock
        display4 = _emptyTransientBlock
        display5 = _emptyTransientBlock

      case 7 =>
        if (transient) normalize(7)

        if (depth == 6) {
          depth = 7
          display7 = makeNewRoot1(display6, 7)
        } else {
          val newRoot = copyAndIncLeftRoot(display7, transient, 7)
          if (transient) {
            withRecomputedSizes(newRoot, 7, 1)
            newRoot.update(1, display6)
          }
          display7 = newRoot
        }
        display0 = new Leaf(1)
        val _emptyTransientBlock = new Node(2) //emptyTransientBlock
        display1 = _emptyTransientBlock
        display2 = _emptyTransientBlock
        display3 = _emptyTransientBlock
        display4 = _emptyTransientBlock
        display5 = _emptyTransientBlock
        display6 = _emptyTransientBlock

      case _ => throw new IllegalStateException()
    }
  }

  final private[Immutable] def gotoPos(index: Int, xor: Int): Unit = {
    if (xor < (1 << 5)) ()
    else if (xor < (1 << 10))
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
    else if (xor < (1 << 15)) {
      display1 = display2(index >> 10 & 31).asInstanceOf[Node]
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
    } else if (xor < (1 << 20)) {
      display2 = display3(index >> 15 & 31).asInstanceOf[Node]
      display1 = display2(index >> 10 & 31).asInstanceOf[Node]
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
    } else if (xor < (1 << 25)) {
      display3 = display4(index >> 20 & 31).asInstanceOf[Node]
      display2 = display3(index >> 15 & 31).asInstanceOf[Node]
      display1 = display2(index >> 10 & 31).asInstanceOf[Node]
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
    } else if (xor < (1 << 30)) {
      display4 = display5(index >> 25 & 31).asInstanceOf[Node]
      display3 = display4(index >> 20 & 31).asInstanceOf[Node]
      display2 = display3(index >> 15 & 31).asInstanceOf[Node]
      display1 = display2(index >> 10 & 31).asInstanceOf[Node]
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
    } else if (xor < (1 << 35)) {
      display5 = display6(index >> 30 & 31).asInstanceOf[Node]
      display4 = display5(index >> 25 & 31).asInstanceOf[Node]
      display3 = display4(index >> 20 & 31).asInstanceOf[Node]
      display2 = display3(index >> 15 & 31).asInstanceOf[Node]
      display1 = display2(index >> 10 & 31).asInstanceOf[Node]
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
    } else if (xor < (1 << 40)) {
      display6 = display7(index >> 35 & 31).asInstanceOf[Node]
      display5 = display6(index >> 30 & 31).asInstanceOf[Node]
      display4 = display5(index >> 25 & 31).asInstanceOf[Node]
      display3 = display4(index >> 20 & 31).asInstanceOf[Node]
      display2 = display3(index >> 15 & 31).asInstanceOf[Node]
      display1 = display2(index >> 10 & 31).asInstanceOf[Node]
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
    } else
      throw new IllegalArgumentException(index.toString)
  }

  final private[Immutable] def gotoNextBlockStart(index: Int,
                                                  xor: Int): Unit = {
    if (xor >= (1 << 10)) {
      if (xor >= (1 << 15)) {
        if (xor >= (1 << 20)) {
          if (xor >= (1 << 25)) {
            if (xor >= (1 << 30)) {
              if (xor >= (1 << 35)) {
                if (xor >= (1 << 40)) {
                  throw new IllegalArgumentException()
                } else
                  display6 = display7(index >> 35 & 31).asInstanceOf[Node]
              } else
                display5 = display6(index >> 30 & 31).asInstanceOf[Node]
            } else
              display4 = display5(index >> 25 & 31).asInstanceOf[Node]
          } else
            display3 = display4(index >> 20 & 31).asInstanceOf[Node]
        } else
          display2 = display3(index >> 15 & 31).asInstanceOf[Node]
      } else
        display1 = display2(index >> 10 & 31).asInstanceOf[Node]
    } else
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
  }

  final private[Immutable] def gotoPrevBlockStart(index: Int,
                                                  xor: Int): Unit = {
    if (xor >= (1 << 10)) {
      if (xor >= (1 << 15)) {
        if (xor >= (1 << 20)) {
          if (xor >= (1 << 25)) {
            if (xor >= (1 << 30)) {
              if (xor >= (1 << 35)) {
                if (xor >= (1 << 40)) {
                  throw new IllegalArgumentException()
                } else
                  display6 = display7(index >> 35 & 31).asInstanceOf[Node]
              } else
                display5 = display6(index >> 30 & 31).asInstanceOf[Node]
            } else
              display4 = display5(index >> 25 & 31).asInstanceOf[Node]
          } else
            display3 = display4(index >> 20 & 31).asInstanceOf[Node]
        } else
          display2 = display3(index >> 15 & 31).asInstanceOf[Node]
      } else
        display1 = display2(index >> 10 & 31).asInstanceOf[Node]
    } else
      display0 = display1(index >> 5 & 31).asInstanceOf[Leaf]
  }

  final private[Immutable] def gotoNextBlockStartWritable(index: Int,
                                                          xor: Int)(implicit ct: ClassTag[A]): Unit = {
    if (xor < (1 << 10)) {

      display0 = new Leaf(32)
      display1.update(index >> 5 & 31, display0)

    } else if (xor < (1 << 15)) {

      if (depth == 1) {
        display2 = new Node(33)
        display2.update(0, display1)
        depth += 1
      }
      display0 = new Leaf(32)
      display1 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)

    } else if (xor < (1 << 20)) {

      if (depth == 2) {
        display3 = new Node(33)
        display3.update(0, display2)
        depth += 1
      }
      display0 = new Leaf(32)
      display1 = new Node(33)
      display2 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
      display3.update(index >> 15 & 31, display2)

    } else if (xor < (1 << 25)) {

      if (depth == 3) {
        display4 = new Node(33)
        display4.update(0, display3)
        depth += 1
      }
      display0 = new Leaf(32)
      display1 = new Node(33)
      display2 = new Node(33)
      display3 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
      display3.update(index >> 15 & 31, display2)
      display4.update(index >> 20 & 31, display3)

    } else if (xor < (1 << 30)) {

      if (depth == 4) {
        display5 = new Node(33)
        display5.update(0, display4)
        depth += 1
      }
      display0 = new Leaf(32)
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

      if (depth == 5) {
        display6 = new Node(33)
        display6.update(0, display5)
        depth += 1
      }
      display0 = new Leaf(32)
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

    } else if (xor < (1 << 40)) {

      if (depth == 6) {
        display7 = new Node(33)
        display7.update(0, display6)
        depth += 1
      }
      display0 = new Leaf(32)
      display1 = new Node(33)
      display2 = new Node(33)
      display3 = new Node(33)
      display4 = new Node(33)
      display5 = new Node(33)
      display6 = new Node(33)
      display1.update(index >> 5 & 31, display0)
      display2.update(index >> 10 & 31, display1)
      display3.update(index >> 15 & 31, display2)
      display4.update(index >> 20 & 31, display3)
      display5.update(index >> 25 & 31, display4)
      display6.update(index >> 30 & 31, display5)
      display7.update(index >> 35 & 31, display6)

    } else
      throw new IllegalArgumentException()
  }


  final private[Immutable] def normalize(_depth: Int): Unit = {

    val _focusDepth: Int = focusDepth
    val stabilizationIndex: Int = focus | focusRelax
    copyDisplaysAndStabilizeDisplayPath(_focusDepth, stabilizationIndex)

    var currentLevel: Int = _focusDepth
    if (currentLevel < _depth) {
      var display = currentLevel match {
        case 1 => display1
        case 2 => display2
        case 3 => display3
        case 4 => display4
        case 5 => display5
        case 6 => display6
        case 7 => display7
      }
      do {
        val newDisplay: Node = copyOf(display)
        val index: Int = stabilizationIndex >> (5 * currentLevel) & 31
        currentLevel match {

          case 1 =>
            newDisplay.update(index, display0)
            display1 = withRecomputedSizes(newDisplay, 1, index)
            display = display2

          case 2 =>
            newDisplay.update(index, display1)
            display2 = withRecomputedSizes(newDisplay, 2, index)
            display = display3

          case 3 =>
            newDisplay.update(index, display2)
            display3 = withRecomputedSizes(newDisplay, 3, index)
            display = display4

          case 4 =>
            newDisplay.update(index, display3)
            display4 = withRecomputedSizes(newDisplay, 4, index)
            display = display5

          case 5 =>
            newDisplay.update(index, display4)
            display5 = withRecomputedSizes(newDisplay, 5, index)
            display = display6

          case 6 =>
            newDisplay.update(index, display5)
            display6 = withRecomputedSizes(newDisplay, 6, index)
            display = display7

          case 7 =>
            newDisplay.update(index, display6)
            display7 = withRecomputedSizes(newDisplay, 7, index)

        }
        currentLevel += 1
      } while (currentLevel < _depth)
    }
  }

  final private[Immutable] def copyDisplays(_depth: Int, _focus: Int): Unit = {
    if (1 <= _depth) {
      if (2 <= _depth) {
        if (3 <= _depth) {
          if (4 <= _depth) {
            if (5 <= _depth) {
              if (6 <= _depth) {
                if (7 <= _depth) {
                  val idx7 = _focus >> 35 & 31 + 1
                  display7 = copyOf(display7, idx7, idx7 + 1)
                }
                val idx6 = _focus >> 30 & 31 + 1
                display6 = copyOf(display6, idx6, idx6 + 1)
              }
              val idx5 = _focus >> 25 & 31 + 1
              display5 = copyOf(display5, idx5, idx5 + 1)
            }
            val idx4 = _focus >> 20 & 31 + 1
            display4 = copyOf(display4, idx4, idx4 + 1)
          }
          val idx3 = _focus >> 15 & 31 + 1
          display3 = copyOf(display3, idx3, idx3 + 1)
        }
        val idx2 = _focus >> 10 & 31 + 1
        display2 = copyOf(display2, idx2, idx2 + 1)
      }
      val idx1 = _focus >> 5 & 31 + 1
      display1 = copyOf(display1, idx1, idx1 + 1)
    }
  }

  final private[Immutable] def copyDisplaysAndNullFocusedBranch(_depth: Int,
                                                                _focus: Int): Unit = _depth match {
    // can also be written using if statements like the previous function CopyDisplays
    case 1 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)

    case 2 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)

    case 3 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)

    case 4 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)
      display4 = copyOfAndNull(display4, _focus >> 20 & 31)

    case 5 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)
      display4 = copyOfAndNull(display4, _focus >> 20 & 31)
      display5 = copyOfAndNull(display5, _focus >> 25 & 31)

    case 6 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)
      display4 = copyOfAndNull(display4, _focus >> 20 & 31)
      display5 = copyOfAndNull(display5, _focus >> 25 & 31)
      display6 = copyOfAndNull(display6, _focus >> 30 & 31)

    case 7 =>
      display1 = copyOfAndNull(display1, _focus >> 5 & 31)
      display2 = copyOfAndNull(display2, _focus >> 10 & 31)
      display3 = copyOfAndNull(display3, _focus >> 15 & 31)
      display4 = copyOfAndNull(display4, _focus >> 20 & 31)
      display5 = copyOfAndNull(display5, _focus >> 25 & 31)
      display6 = copyOfAndNull(display6, _focus >> 30 & 31)
      display7 = copyOfAndNull(display7, _focus >> 35 & 31)

  }

  final private[Immutable] def copyDisplaysAndStabilizeDisplayPath(_depth: Int,
                                                                   _focus: Int): Unit = _depth match {
    case 1 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus >> 5 & 31, display0)
      display1 = d1

    case 2 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus >> 5 & 31, display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus >> 10 & 31, d1)
      display2 = d2

    case 3 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus >> 5 & 31, display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus >> 10 & 31, d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus >> 15 & 31, d2)
      display3 = d3

    case 4 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus >> 5 & 31, display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus >> 10 & 31, d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus >> 15 & 31, d2)
      display3 = d3
      val d4: Node = copyOf(display4)
      d4.update(_focus >> 20 & 31, d3)
      display4 = d4

    case 5 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus >> 5 & 31, display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus >> 10 & 31, d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus >> 15 & 31, d2)
      display3 = d3
      val d4: Node = copyOf(display4)
      d4.update(_focus >> 20 & 31, d3)
      display4 = d4
      val d5: Node = copyOf(display5)
      d5.update(_focus >> 25 & 31, d4)
      display5 = d5

    case 6 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus >> 5 & 31, display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus >> 10 & 31, d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus >> 15 & 31, d2)
      display3 = d3
      val d4: Node = copyOf(display4)
      d4.update(_focus >> 20 & 31, d3)
      display4 = d4
      val d5: Node = copyOf(display5)
      d5.update(_focus >> 25 & 31, d4)
      display5 = d5
      val d6: Node = copyOf(display6)
      d6.update(_focus >> 30 & 31, d5)
      display6 = d6

    case 7 =>
      val d1: Node = copyOf(display1)
      d1.update(_focus >> 5 & 31, display0)
      display1 = d1
      val d2: Node = copyOf(display2)
      d2.update(_focus >> 10 & 31, d1)
      display2 = d2
      val d3: Node = copyOf(display3)
      d3.update(_focus >> 15 & 31, d2)
      display3 = d3
      val d4: Node = copyOf(display4)
      d4.update(_focus >> 20 & 31, d3)
      display4 = d4
      val d5: Node = copyOf(display5)
      d5.update(_focus >> 25 & 31, d4)
      display5 = d5
      val d6: Node = copyOf(display6)
      d6.update(_focus >> 30 & 31, d5)
      display6 = d6
      val d7: Node = copyOf(display7)
      d7.update(_focus >> 35 & 31, d6)
      display7 = d7

  }

  final private[Immutable] def copyDisplaysTop(currentDepth: Int,
                                               _focusRelax: Int): Unit = {
    var _currentDepth = currentDepth
    while (_currentDepth < this.depth) {
      _currentDepth match {

        case 1 =>
          val cutIndex = _focusRelax >> 5 & 31
          display1 = copyOf(display1, cutIndex + 1, cutIndex + 2)

        case 2 =>
          val cutIndex = _focusRelax >> 10 & 31
          display2 = copyOf(display2, cutIndex + 1, cutIndex + 2)

        case 3 =>
          val cutIndex = _focusRelax >> 15 & 31
          display3 = copyOf(display3, cutIndex + 1, cutIndex + 2)

        case 4 =>
          val cutIndex = _focusRelax >> 20 & 31
          display4 = copyOf(display4, cutIndex + 1, cutIndex + 2)

        case 5 =>
          val cutIndex = _focusRelax >> 25 & 31
          display5 = copyOf(display5, cutIndex + 1, cutIndex + 2)

        case 6 =>
          val cutIndex = _focusRelax >> 30 & 31
          display6 = copyOf(display6, cutIndex + 1, cutIndex + 2)

        case 7 =>
          val cutIndex = _focusRelax >> 35 & 31
          display7 = copyOf(display7, cutIndex + 1, cutIndex + 2)

        case _ => throw new IllegalStateException()

      }
      _currentDepth += 1
    }

  }

  final private[Immutable] def stabilizeDisplayPath(_depth: Int,
                                                    _focus: Int): Unit = {
    _depth match {

      case 1 =>
        val d1: Node = display1
        d1.update(_focus >> 5 & 31, display0)

      case 2 =>
        val d1: Node = display1
        d1.update(_focus >> 5 & 31, display0)
        val d2: Node = display2
        d2.update(_focus >> 10 & 31, d1)

      case 3 =>
        val d1: Node = display1
        d1.update(_focus >> 5 & 31, display0)
        val d2: Node = display2
        d2.update(_focus >> 10 & 31, d1)
        val d3: Node = display3
        d3.update(_focus >> 15 & 31, d2)

      case 4 =>
        val d1: Node = display1
        d1.update(_focus >> 5 & 31, display0)
        val d2: Node = display2
        d2.update(_focus >> 10 & 31, d1)
        val d3: Node = display3
        d3.update(_focus >> 15 & 31, d2)
        val d4: Node = display4
        d4.update(_focus >> 20 & 31, d3)

      case 5 =>
        val d1: Node = display1
        d1.update(_focus >> 5 & 31, display0)
        val d2: Node = display2
        d2.update(_focus >> 10 & 31, d1)
        val d3: Node = display3
        d3.update(_focus >> 15 & 31, d2)
        val d4: Node = display4
        d4.update(_focus >> 20 & 31, d3)
        val d5: Node = display5
        d5.update(_focus >> 25 & 31, d4)

      case 6 =>
        val d1 = display1
        d1.update(_focus >> 5 & 31, display0)
        val d2 = display2
        d2.update(_focus >> 10 & 31, d1)
        val d3 = display3
        d3.update(_focus >> 15 & 31, d2)
        val d4 = display4
        d4.update(_focus >> 20 & 31, d3)
        val d5 = display5
        d5.update(_focus >> 25 & 31, d4)
        val d6 = display6
        d6.update(_focus >> 30 & 31, d5)

      case 7 =>
        val d1 = display1
        d1.update(_focus >> 5 & 31, display0)
        val d2 = display2
        d2.update(_focus >> 10 & 31, d1)
        val d3 = display3
        d3.update(_focus >> 15 & 31, d2)
        val d4 = display4
        d4.update(_focus >> 20 & 31, d3)
        val d5 = display5
        d5.update(_focus >> 25 & 31, d4)
        val d6 = display6
        d6.update(_focus >> 30 & 31, d5)
        val d7 = display7
        d7.update(_focus >> 35 & 31, d6)

    }
  }

  private[Immutable] def cleanTopTake(cutIndex: Int): Unit = {
    this.depth match {

      case 1 =>
        if ((cutIndex >> 5) == 0) {
          display1 = null
          this.depth = 0
        } else
          this.depth = 1

      case 2 =>
        if ((cutIndex >> 10) == 0) {
          display2 = null
          if ((cutIndex >> 5) == 0) {
            display1 = null
            this.depth = 0
          } else
            this.depth = 1
        } else
          this.depth = 2

      case 3 =>
        if ((cutIndex >> 15) == 0) {
          display3 = null
          if ((cutIndex >> 10) == 0) {
            display2 = null
            if ((cutIndex >> 5) == 0) {
              display1 = null
              this.depth = 0
            } else
              this.depth = 1
          } else
            this.depth = 2
        } else
          this.depth = 3

      case 4 =>
        if ((cutIndex >> 20) == 0) {
          display4 = null
          if ((cutIndex >> 15) == 0) {
            display3 = null
            if ((cutIndex >> 10) == 0) {
              display2 = null
              if ((cutIndex >> 5) == 0) {
                display1 = null
                this.depth = 0
              } else
                this.depth = 1
            } else
              this.depth = 2
          } else
            this.depth = 3
        } else
          this.depth = 4

      case 5 =>
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
                  this.depth = 0
                } else
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
                    this.depth = 0
                  } else
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
        if ((cutIndex >> 35) == 0) {
          display7 = null
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
                      this.depth = 0
                    } else
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
  }

  private[Immutable] def cleanTopDrop(cutIndex: Int): Unit = {
    this.depth match {

      case 1 =>
        if ((cutIndex >> 5) == (display1.length - 2)) {
          display1 = null
          this.depth = 0
        } else
          this.depth = 1

      case 2 =>
        if ((cutIndex >> 10) == (display2.length - 2)) {
          display2 = null
          if ((cutIndex >> 5) == (display1.length - 2)) {
            display1 = null
            this.depth = 0
          } else
            this.depth = 1
        } else
          this.depth = 2

      case 3 =>
        if ((cutIndex >> 15) == (display3.length - 2)) {
          display3 = null
          if ((cutIndex >> 10) == (display2.length - 2)) {
            display2 = null
            if ((cutIndex >> 5) == (display1.length - 2)) {
              display1 = null
              this.depth = 0
            } else
              this.depth = 1
          } else
            this.depth = 2
        } else
          this.depth = 3

      case 4 =>
        if ((cutIndex >> 20) == (display4.length - 2)) {
          display4 = null
          if ((cutIndex >> 15) == (display3.length - 2)) {
            display3 = null
            if ((cutIndex >> 10) == (display2.length - 2)) {
              display2 = null
              if ((cutIndex >> 5) == (display1.length - 2)) {
                display1 = null
                this.depth = 0
              } else
                this.depth = 1
            } else
              this.depth = 2
          } else
            this.depth = 3
        } else
          this.depth = 4

      case 5 =>
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
                  this.depth = 0
                } else
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
                    this.depth = 0
                  } else
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
        if ((cutIndex >> 35) == (display7.length - 2)) {
          display7 = null
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
                      this.depth = 0
                    } else
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
  }

  final private[Immutable] def copyOf(node: Node): Node = {
    val length: Int = node.length
    val newArray: Node = new Node(length)
    System.arraycopy(node, 0, newArray, 0, length)
    newArray
  }

  final private[Immutable] def copyOf(node: Node,
                                      numElements: Int,
                                      newSize: Int): Node = {
    val newArray: Node = new Node(newSize)
    System.arraycopy(node, 0, newArray, 0, numElements)
    newArray
  }

  final private[Immutable] def copyOf(leaf: Leaf)(implicit ct: ClassTag[A]): Leaf = {
    val length: Int = leaf.length
    val newArray: Leaf = new Leaf(length)
    System.arraycopy(leaf, 0, newArray, 0, length)
    newArray
  }

  final private[Immutable] def copyOf(array: Leaf,
                                      numElements: Int,
                                      newSize: Int)(implicit ct: ClassTag[A]): Leaf = {
    val newArray: Leaf = new Leaf(newSize)
    System.arraycopy(array, 0, newArray, 0, numElements)
    newArray
  }

  final private[Immutable] def copyOfAndNull(node: Node,
                                             nullIndex: Int): Node = {
    val length: Int = node.length
    val newNode: Node = new Node(length)
    // length - 1 since the last location has the sizes array
    System.arraycopy(node, 0, newNode, 0, length - 1)
    newNode.update(nullIndex, null)

    val sizes: Size = node(length - 1).asInstanceOf[Size]
    if (sizes != null)
      newNode.update(length - 1, makeTransientSizes(sizes, nullIndex))
    newNode
  }

  final private def makeNewRoot0(node: Node): Node = {
    val newRoot: Node = new Node(3)
    newRoot.update(0, node)
    val dSizes: Size = node(node.length - 1).asInstanceOf[Size]

    if (dSizes != null) {
      val newRootSizes: Size = new Size(2)
      val dSize: Int = dSizes(node.length - 2)
      newRootSizes.update(0, dSize)
      newRootSizes.update(1, dSize)
      newRoot.update(2, newRootSizes)
    }
    newRoot
  }

  final private def makeNewRoot1(node: Node,
                                 currentDepth: Int): Node = {
    val dSize: Int = treeSize(node, currentDepth - 1)
    val newRootSizes: Size = new Size(2)
    newRootSizes.update(1, dSize)
    val newRoot: Node = new Node(3)
    newRoot.update(1, node)
    newRoot.update(2, newRootSizes)
    newRoot
  }

  final private[Immutable] def makeTransientSizes(oldSizes: Size,
                                                  transientBranchIndex: Int): Size = {
    val newSizes: Size = new Size(oldSizes.length)
    var delta: Int = oldSizes(transientBranchIndex)
    if (transientBranchIndex > 0) {
      delta -= oldSizes(transientBranchIndex - 1)
      if (!oldSizes.eq(newSizes))
        System.arraycopy(oldSizes, 0, newSizes, 0, transientBranchIndex)
    }

    var i: Int = transientBranchIndex
    while (i < newSizes.length) {
      newSizes.update(i, oldSizes(i) - delta)
      i += 1
    }
    newSizes
  }

  final private def copyAndIncRightRoot(node: Node,
                                        transient: Boolean,
                                        currentLevel: Int): Node = {
    val length: Int = node.length
    // length - 1 since the last position of node has the sizes array
    val newRoot: Node = copyOf(node, length - 1, length + 1)
    val oldSizes: Size = node(length - 1).asInstanceOf[Size]
    if (oldSizes != null) {
      val newSizes: Size = new Size(length)
      System.arraycopy(oldSizes, 0, newSizes, 0, length - 1)
      if (transient)
        newSizes.update(length - 1, 1 << (5 * currentLevel))
      // At position length - 1 since the last position
      // needs to be empty for the last element of the node
      newSizes.update(length - 1, newSizes(length - 2))
      newRoot.update(length, newSizes)
    }
    newRoot
  }

  final private def copyAndIncLeftRoot(node: Node,
                                       transient: Boolean,
                                       currentLevel: Int): Node = {
    val length: Int = node.length
    val newRoot: Node = new Node(length + 1)
    // Copy existing contents to the new array, but shifted by one to the left
    System.arraycopy(node, 0, newRoot, 1, length - 1)

    val oldSizes: Size = node(length - 1).asInstanceOf[Size]
    val newSizes: Size = new Size(length)

    if (oldSizes != null) {
      if (transient) {
        System.arraycopy(oldSizes, 1, newSizes, 2, length - 2)
      }
      else {
        System.arraycopy(oldSizes, 0, newSizes, 1, length - 1)
      }
    } else {
      val subTreeSize: Int = 1 << (5 * currentLevel)
      var acc = 0
      var i = 1
      while (i < length - 1) {
        acc += subTreeSize
        newSizes.update(i, acc)
        i += 1
      }
      newSizes.update(i, acc + treeSize(node(node.length - 2).asInstanceOf[Node], currentLevel))
    }
    newRoot.update(length, newSizes)
    newRoot
  }

  final private[Immutable] def withComputedSizes(node: Node,
                                                 currentDepth: Int): Node = {
    var i: Int = 0
    var acc: Int = 0
    val length: Int = node.length
    if (length > 0) {
      val sizes: Size = new Size(length)
      if (currentDepth == 1) {
        while (i < length) {
          acc += node(i).asInstanceOf[Leaf].length
          sizes.update(i, acc)
          i += 1
        }
        node.update(length - 1, sizes)
      } else if (length == 1 && currentDepth > 1) {
        val child: Node = node(0).asInstanceOf[Node]
        val childSizes: Size = child(child.length - 1).asInstanceOf[Size]
        if (childSizes.length > 1) {
          sizes.update(0, childSizes(childSizes.length - 1))
          node.update(length - 1, sizes)
        } else {
          node.update(length - 1, childSizes)
        }
      } else {
        while (i < length) {
          acc += treeSize(node(i).asInstanceOf[Node], currentDepth - 1)
          sizes.update(i, acc)
          i += 1
        }
        node.update(length - 1, sizes)
      }
    }
    node
  }

  final private def withRecomputedSizes(node: Node,
                                        currentDepth: Int,
                                        branchToUpdate: Int): Node = {
    val endIndex: Int = node.length - 1
    val oldSizes: Size = node(endIndex).asInstanceOf[Size]
    val newSizes: Size = new Size(node.length)
    val delta: Int = {
      if (currentDepth == 1)
        node(branchToUpdate).asInstanceOf[Leaf].length
      else
        treeSize(node(branchToUpdate).asInstanceOf[Node], currentDepth - 1)
    }

    if (branchToUpdate > 0) System.arraycopy(oldSizes, 0, newSizes, 0, branchToUpdate)
    var i: Int = branchToUpdate
    while (i < endIndex) {
      newSizes.update(i, oldSizes(i) + delta)
      i += 1
    }
    node.update(endIndex, newSizes)

    node
  }

  @inline final private def isBalanced(node: Node,
                                       sizes: Size,
                                       currentDepth: Int): Boolean = {
    // Either root is of length one or in all except for the last location, the subtrees are full
    if (node.length == 1) false else sizes(node.length - 2) == ((node.length - 1) << (5 * currentDepth))
  }

  final private def treeSize(tree: Node, currentDepth: Int): Int = {
    @tailrec def treeSizeRec(node: Node, currentDepth: Int, acc: Int): Int = {
      val length: Int = node.length
      val treeSizes: Size = node(length - 1).asInstanceOf[Size]
      if (currentDepth == 1) {
        acc + treeSizes(treeSizes.length - 1)
      } else {
        treeSizeRec(node(length - 2).asInstanceOf[Node], currentDepth - 1, acc + treeSizes(treeSizes.length - 1))
      }
    }
    treeSizeRec(tree, currentDepth, 0)
  }

  final private[Immutable] def getElem(index: Int, xor: Int): A = {
    if (xor < (1 << 5))
      getElem0(display0, index)
    else if (xor < (1 << 10))
      getElem1(display1, index)
    else if (xor < (1 << 15))
      getElem2(display2, index)
    else if (xor < (1 << 20))
      getElem3(display3, index)
    else if (xor < (1 << 25))
      getElem4(display4, index)
    else if (xor < (1 << 30))
      getElem5(display5, index)
    else if (xor < (1 << 35))
      getElem6(display6, index)
    else if (xor < (1 << 40))
      getElem7(display7, index)
    else
      throw new IllegalArgumentException(xor.toString)
  }

  final private def getElem0(block: Leaf, index: Int): A =
    block(index & 31)

  final private def getElem1(block: Node, index: Int): A =
    block(index >> 5 & 31)
      .asInstanceOf[Leaf](index & 31)
      .asInstanceOf[A]

  final private def getElem2(block: Node, index: Int): A =
    block(index >> 10 & 31)
      .asInstanceOf[Node](index >> 5 & 31)
      .asInstanceOf[Leaf](index & 31)
      .asInstanceOf[A]

  final private def getElem3(block: Node, index: Int): A =
    block(index >> 15 & 31)
      .asInstanceOf[Node](index >> 10 & 31)
      .asInstanceOf[Node](index >> 5 & 31)
      .asInstanceOf[Leaf](index & 31)
      .asInstanceOf[A]

  final private def getElem4(block: Node, index: Int): A =
    block(index >> 20 & 31)
      .asInstanceOf[Node](index >> 15 & 31)
      .asInstanceOf[Node](index >> 10 & 31)
      .asInstanceOf[Node](index >> 5 & 31)
      .asInstanceOf[Leaf](index & 31)
      .asInstanceOf[A]

  final private def getElem5(block: Node, index: Int): A =
    block(index >> 25 & 31)
      .asInstanceOf[Node](index >> 20 & 31)
      .asInstanceOf[Node](index >> 15 & 31)
      .asInstanceOf[Node](index >> 10 & 31)
      .asInstanceOf[Node](index >> 5 & 31)
      .asInstanceOf[Leaf](index & 31)
      .asInstanceOf[A]

  final private def getElem6(block: Node, index: Int): A =
    block(index >> 30 & 31)
      .asInstanceOf[Node](index >> 25 & 31)
      .asInstanceOf[Node](index >> 20 & 31)
      .asInstanceOf[Node](index >> 15 & 31)
      .asInstanceOf[Node](index >> 10 & 31)
      .asInstanceOf[Node](index >> 5 & 31)
      .asInstanceOf[Leaf](index & 31)
      .asInstanceOf[A]

  final private def getElem7(block: Node, index: Int): A =
    block(index >> 35 & 31)
      .asInstanceOf[Node](index >> 30 & 31)
      .asInstanceOf[Node](index >> 25 & 31)
      .asInstanceOf[Node](index >> 20 & 31)
      .asInstanceOf[Node](index >> 15 & 31)
      .asInstanceOf[Node](index >> 10 & 31)
      .asInstanceOf[Node](index >> 5 & 31)
      .asInstanceOf[Leaf](index & 31)
      .asInstanceOf[A]

  private[Immutable] def debugToString(): String = {
    val sb = new StringBuilder()
    sb.append("Vector (\n")
    sb.append(
      "\t"
        .+("display0")
        .+(" = ")
        .+(display0)
        .+(" ")
        .+(
          if (display0 != null)
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
    sb.append(
      "\t"
        .+("display7")
        .+(" = ")
        .+(display7)
        .+(" ")
        .+(
          if (display7.!=(null))
            display7.mkString("[", ", ", "]")
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