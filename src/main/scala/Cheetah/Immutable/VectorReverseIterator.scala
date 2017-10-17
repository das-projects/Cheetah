package Cheetah.Immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

class VectorReverseIterator[+A](startIndex: Int, final override private[Immutable] val endIndex: Int)
  extends VectorPointer[A @uncheckedVariance]{
  self =>

  private var lastIndexOfBlock: Int = _
  private var lo: Int = _
  private var endLo: Int = _
  private var _hasNext: Boolean = startIndex < endIndex

  final private[Immutable] def initIteratorFrom[B >: A : ClassTag](that: VectorPointer[B]): Unit = {
    initWithFocusFrom(that.asInstanceOf[VectorPointer[A]])
    _hasNext = startIndex < endIndex
    if (_hasNext) {
      val index: Int = endIndex - 1
      focusOn(index)
      lastIndexOfBlock = index
      lo = (index - focusStart) & 31
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

  def seq: VectorReverseIterator[A] = this

  /** Tests whether this iterator can provide another element.
    *
    *  @return  `true` if a subsequent call to `next` will yield an element,
    *           `false` otherwise.
    *  @note    Reuse: $preservesIterator
    */
  final def hasNext: Boolean = _hasNext

  /** Produces the next element of this iterator.
    *
    *  @return  the next element of this iterator, if `hasNext` is `true`,
    *           undefined behavior otherwise.
    *  @note    Reuse: $preservesIterator
    */
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

  /** Applies a function `f` to all elements of this $coll.
    *
    *  @param  f   the function that is applied for its side-effect to every element.
    *              The result of function `f` is discarded.
    *
    *  @tparam  U  the type parameter describing the result of function `f`.
    *              This result will always be ignored. Typically `U` is `Unit`,
    *              but this is not necessary.
    *
    *  @usecase def foreach(f: A => Unit): Unit
    *    @inheritdoc
    *
    *    Note: this method underlies the implementation of most other bulk operations.
    *    It's important to implement this method in an efficient way.
    *
    */
  def foreach[U](f: A => U): Unit = {
    while (this.hasNext) f(this.next())
  }

  /** The size of this $coll.
    *
    *  $willNotTerminateInf
    *
    *  @return    the number of elements in this $coll.
    */
  def size: Int = {
    var result = 0
    self.foreach(_ => result += 1)
    result
  }

  /** The size of this $coll, if it can be cheaply computed
    *
    *  @return    the number of elements in this $coll, or -1 if the size cannot be determined cheaply
    */

  protected[Immutable] def sizeHintIfCheap: Int = -1

  /** Tests whether the $coll is empty.
    *
    *  Note: Implementations in subclasses that are not repeatedly traversable must take
    *  care not to consume any elements when `isEmpty` is called.
    *
    *  @return    `true` if the $coll contains no elements, `false` otherwise.
    */
  def isEmpty: Boolean = !hasNext

  /** Tests whether the $coll is not empty.
    *
    *  @return    `true` if the $coll contains at least one element, `false` otherwise.
    */
  def nonEmpty: Boolean = hasNext

  /** Tests whether this $coll can be repeatedly traversed.  Always
    *  true for Traversables and false for Iterators unless overridden.
    *
    *  @return   `true` if it is repeatedly traversable, `false` otherwise.
    */
  def isTraversableAgain: Boolean = false

  /** Tests whether this Iterator has a known size.
    *
    *  @return   `true` for empty Iterators, `false` otherwise.
    *  @note     Reuse: $preservesIterator
    */

  def hasDefiniteSize: Boolean = isEmpty

  /** Reduces the elements of this $coll using the specified associative binary operator.
    *
    *  $undefinedorder
    *
    *  @tparam B      A type parameter for the binary operator, a supertype of `A`.
    *  @param op       A binary operator that must be associative.
    *  @return         The result of applying reduce operator `op` between all the elements if the $coll is nonempty.
    *  @throws UnsupportedOperationException
    *  if this $coll is empty.
    */
  def reduce[B >: A](op: (B, B) => B): B = reduceLeft(op)

  /** Reduces the elements of this $coll, if any, using the specified
    *  associative binary operator.
    *
    *  $undefinedorder
    *
    *  @tparam B     A type parameter for the binary operator, a supertype of `A`.
    *  @param op      A binary operator that must be associative.
    *  @return        An option value containing result of applying reduce operator `op` between all
    *                 the elements if the collection is nonempty, and `None` otherwise.
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = reduceLeftOption(op)

  /** Folds the elements of this $coll using the specified associative
    *  binary operator.
    *
    *  $undefinedorder
    *  $willNotTerminateInf
    *
    *  @tparam B     a type parameter for the binary operator, a supertype of `A`.
    *  @param z       a neutral element for the fold operation; may be added to the result
    *                 an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
    *                 0 for addition, or 1 for multiplication).
    *  @param op      a binary operator that must be associative.
    *  @return        the result of applying the fold operator `op` between all the elements and `z`, or `z` if this $coll is empty.
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B = foldLeft(z)(op)

  /** Applies a binary operator to a start value and all elements of this $coll,
    *  going left to right.
    *
    *  Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the same as
    *  `xs foldLeft z`.
    *
    *  Examples:
    *
    *  Note that the folding function used to compute b is equivalent to that used to compute c.
    *  {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = (5 /: a)(_+_)
    *      b: Int = 15
    *
    *      scala> val c = (5 /: a)((x,y) => x + y)
    *      c: Int = 15
    *  }}}

    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going left to right with the start value `z` on the left:
    *           {{{
    *             op(...op(op(z, x_1), x_2), ..., x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    */
  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** Applies a binary operator to all elements of this $coll and a start value,
    *  going right to left.
    *
    *  Note: `:\` is alternate syntax for `foldRight`; `xs :\ z` is the same as
    *  `xs foldRight z`.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  Examples:
    *
    *  Note that the folding function used to compute b is equivalent to that used to compute c.
    *  {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = (a :\ 5)(_+_)
    *      b: Int = 15
    *
    *      scala> val c = (a :\ 5)((x,y) => x + y)
    *      c: Int = 15
    *
    *  }}}
    *
    *  @param   z    the start value
    *  @param   op   the binary operator
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going right to left with the start value `z` on the right:
    *           {{{
    *             op(x_1, op(x_2, ... op(x_n, z)...))
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    */
  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Applies a binary operator to a start value and all elements of this $coll,
    *  going left to right.
    *
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going left to right with the start value `z` on the left:
    *           {{{
    *             op(...op(z, x_1), x_2, ..., x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *           Returns `z` if this $coll is empty.
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result: B = z
    this.foreach(x => result = op(result, x))
    result
  }

  /** Applies a binary operator to all elements of this $coll and a start value,
    *  going right to left.
    *
    *  $willNotTerminateInf
    *  $orderDependentFold
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going right to left with the start value `z` on the right:
    *           {{{
    *             op(x_1, op(x_2, ... op(x_n, z)...))
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *           Returns `z` if this $coll is empty.
    */
  def foldRight[B](z: B)(op: (A, B) => B): B =
    reversed.foldLeft(z)((x, y) => op(y, x))

  // for internal use
  protected[this] def reversed: List[A] = {
    var elems: List[A] = Nil
    self.foreach(elems ::= _)
    elems
  }

  /** Aggregates the results of applying an operator to subsequent elements.
    *
    *  This is a more general form of `fold` and `reduce`. It is similar to
    *  `foldLeft` in that it doesn't require the result to be a supertype of the
    *  element type. In addition, it allows parallel collections to be processed
    *  in chunks, and then combines the intermediate results.
    *
    *  `aggregate` splits the $coll into partitions and processes each
    *  partition by sequentially applying `seqop`, starting with `z` (like
    *  `foldLeft`). Those intermediate results are then combined by using
    *  `combop` (like `fold`). The implementation of this operation may operate
    *  on an arbitrary number of collection partitions (even 1), so `combop` may
    *  be invoked an arbitrary number of times (even 0).
    *
    *  As an example, consider summing up the integer values of a list of chars.
    *  The initial value for the sum is 0. First, `seqop` transforms each input
    *  character to an Int and adds it to the sum (of the partition). Then,
    *  `combop` just needs to sum up the intermediate results of the partitions:
    *  {{{
    *    List('a', 'b', 'c').aggregate(0)({ (sum, ch) => sum + ch.toInt }, { (p1, p2) => p1 + p2 })
    *  }}}
    *
    *  @tparam B        the type of accumulated results
    *  @param z         the initial value for the accumulated result of the partition - this
    *                   will typically be the neutral element for the `seqop` operator (e.g.
    *                   `Nil` for list concatenation or `0` for summation) and may be evaluated
    *                   more than once
    *  @param seqop     an operator used to accumulate results within a partition
    *  @param combop    an associative operator used to combine results from different partitions
    */
  def aggregate[B](z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)

  /** Applies a binary operator to all elements of this $coll,
    *  going left to right.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going left to right:
    *           {{{
    *             op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *  @throws UnsupportedOperationException if this $coll is empty.   */

  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first: Boolean = true
    var acc: B = 0.asInstanceOf[B]

    self.foreach(x => if(first) {
      acc = x
      first = false
    } else acc = op(acc, x))
    acc
  }


  /** Applies a binary operator to all elements of this $coll, going right to left.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going right to left:
    *           {{{
    *             op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *  @throws UnsupportedOperationException if this $coll is empty.
    */
  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceRight")

    reversed.reduceLeft[B]((x, y) => op(y, x))
  }

  /** Optionally applies a binary operator to all elements of this $coll, going left to right.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  an option value containing the result of `reduceLeft(op)` if this $coll is nonempty,
    *           `None` otherwise.
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (isEmpty) None else Some(reduceLeft(op))


  /** Optionally applies a binary operator to all elements of this $coll, going
    *  right to left.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  an option value containing the result of `reduceRight(op)` if this $coll is nonempty,
    *           `None` otherwise.
    */
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] =
    if (isEmpty) None else Some(reduceRight(op))


  /** Counts the number of elements in the $coll which satisfy a predicate.
    *
    *  @param p     the predicate  used to test elements.
    *  @return      the number of elements satisfying the predicate `p`.
    */
  def count(p: A => Boolean): Int = {
    var cnt: Int = 0
    this.foreach(x => if (p(x)) cnt += 1)
    cnt
  }


  /** Sums up the elements of this collection.
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `+` operator to be used in forming the sum.
    *   @tparam  B   the result type of the `+` operator.
    *   @return       the sum of all elements of this $coll with respect to the `+` operator in `num`.
    *
    *   @usecase def sum: A
    *     @inheritdoc
    *
    *     @return       the sum of all elements in this $coll of numbers of type `Int`.
    *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
    *     can be used as element type of the $coll and as result type of `sum`.
    *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
    *
    */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Multiplies up the elements of this collection.
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `*` operator to be used in forming the product.
    *   @tparam  B   the result type of the `*` operator.
    *   @return       the product of all elements of this $coll with respect to the `*` operator in `num`.
    *
    *   @usecase def product: A
    *     @inheritdoc
    *
    *     @return       the product of all elements in this $coll of numbers of type `Int`.
    *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
    *     can be used as element type of the $coll and as result type of `product`.
    *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
    */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** Finds the smallest element.
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   the smallest element of this $coll with respect to the ordering `ord`.
    *
    *  @usecase def min: A
    *    @inheritdoc
    *
    *    @return   the smallest element of this $coll
    */
  def min[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")

    reduceLeft((x, y) => if (ord.lteq(x, y)) x else y)
  }

  /** Finds the largest element.
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   the largest element of this $coll with respect to the ordering `ord`.
    *
    *  @usecase def max: A
    *    @inheritdoc
    *
    *    @return   the largest element of this $coll.
    */
  def max[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")

    reduceLeft((x, y) => if (ord.gteq(x, y)) x else y)
  }

  /** Finds the first element which yields the largest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   the first element of this $coll with the largest value measured by function f
    *  with respect to the ordering `cmp`.
    *
    *  @usecase def maxBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return   the first element of this $coll with the largest value measured by function f.
    */
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")

    var maxF: B = null.asInstanceOf[B]
    var maxElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- self) {
      val fx = f(elem)
      if (first || cmp.gt(fx, maxF)) {
        maxElem = elem
        maxF = fx
        first = false
      }
    }
    maxElem
  }

  /** Finds the first element which yields the smallest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   the first element of this $coll with the smallest value measured by function f
    *  with respect to the ordering `cmp`.
    *
    *  @usecase def minBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return   the first element of this $coll with the smallest value measured by function f.
    */
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    var minF: B = null.asInstanceOf[B]
    var minElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- self) {
      val fx = f(elem)
      if (first || cmp.lt(fx, minF)) {
        minElem = elem
        minF = fx
        first = false
      }
    }
    minElem
  }

  /** Tests whether a predicate holds for all elements of this $coll.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if this $coll is empty or the given predicate `p`
    *                 holds for all elements of this $coll, otherwise `false`.
    */
  def forall(@deprecatedName('pred) p: A => Boolean): Boolean = {
    var res: Boolean = true
    while (res && this.hasNext) res = p(this.next())
    res
  }

  /** Tests whether a predicate holds for at least one element of this $coll.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if the given predicate `p` is satisfied by at least one element of this $coll, otherwise `false`
    */
  def exists(@deprecatedName('pred) p: A => Boolean): Boolean = {
    var res = false
    while (!res && hasNext) res = p(next())
    res
  }

  /** Tests whether this iterator contains a given value as an element.
    *  $mayNotTerminateInf
    *
    *  @param elem  the element to test.
    *  @return     `true` if this iterator produces some value that is
    *               is equal (as determined by `==`) to `elem`, `false` otherwise.
    *  @note        Reuse: $consumesIterator
    */

  def contains(elem: Any): Boolean = exists(_ == elem)    // Note--this seems faster than manual inlining!

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  $mayNotTerminateInf
    *  $orderDependent
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    */
  def find(@deprecatedName('pred) p: A => Boolean): Option[A] = {
    while (this.hasNext) {
      val a: A = this.next()
      if (p(a)) return Some(a)
    }
    None
  }

  /** Returns the index of the first produced value satisfying a predicate, or -1.
    *  $mayNotTerminateInf
    *
    *  @param  p the predicate to test values
    *  @return   the index of the first produced value satisfying `p`,
    *           or -1 if such an element does not exist until the end of the iterator is reached.
    *  @note    Reuse: $consumesIterator
    */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Returns the index of the first produced value satisfying a predicate, or -1, after or at
    *  some start index.
    *  $mayNotTerminateInf
    *
    *  @param p the predicate to test values
    *  @param from the start index
    *  @return the index `>= from` of the first produced value satisfying `p`,
    *          or -1 if such an element does not exist until the end of the iterator is reached.
    *  @note   Reuse: $consumesIterator
    */
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i: Int = 0
    while (i < from && this.hasNext) {
      this.next()
      i += 1
    }

    while (this.hasNext) {
      if (p(this.next())) return i
      i += 1
    }
    -1
  }

  /** Returns the index of the first occurrence of the specified
    *  object in this iterable object.
    *  $mayNotTerminateInf
    *
    *  @param  elem  element to search for.
    *  @return the index of the first occurrence of `elem` in the values produced by this iterator,
    *          or -1 if such an element does not exist until the end of the iterator is reached.
    *  @note   Reuse: $consumesIterator
    */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Returns the index of the first occurrence of the specified object in this iterable object
    *  after or at some start index.
    *  $mayNotTerminateInf
    *
    *  @param elem element to search for.
    *  @param from the start index
    *  @return the index `>= from` of the first occurrence of `elem` in the values produced by this
    *          iterator, or -1 if such an element does not exist until the end of the iterator is
    *          reached.
    *  @note   Reuse: $consumesIterator
    */
  def indexOf[B >: A](elem: B, from: Int): Int = {
    var i = 0
    while (i < from && this.hasNext) {
      next()
      i += 1
    }

    while (this.hasNext) {
      if (this.next() == elem) return i
      i += 1
    }
    -1
  }

  /** Copies the elements of this $coll to an array.
    *  Fills the given array `xs` with values of this $coll.
    *  Copying will stop once either the end of the current $coll is reached,
    *  or the end of the target array is reached.
    *
    *  @param  xs     the array to fill.
    *  @tparam B      the type of the elements of the target array.
    *
    *  @usecase def copyToArray(xs: Array[A]): Unit
    *    @inheritdoc
    *
    *    $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B]): Unit =
    copyToArray(xs, 0, xs.length)

  /** Copies the elements of this $coll to an array.
    *  Fills the given array `xs` with values of this $coll, beginning at index `start`.
    *  Copying will stop once either the end of the current $coll is reached,
    *  or the end of the target array is reached.
    *
    *  @param  xs     the array to fill.
    *  @param  start  the starting index.
    *  @tparam B      the type of the elements of the target array.
    *
    *  @usecase def copyToArray(xs: Array[A], start: Int): Unit
    *    @inheritdoc
    *
    *    $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    copyToArray(xs, start, xs.length - start)


  /** Copies the elements of this $coll to an array.
    *  Fills the given array `xs` with at most `len` elements of
    *  this $coll, starting at position `start`.
    *  Copying will stop once either the end of the current $coll is reached,
    *  or the end of the target array is reached, or `len` elements have been copied.
    *
    *  @param  xs     the array to fill.
    *  @param  start  the starting index.
    *  @param  len    the maximal number of elements to copy.
    *  @tparam B      the type of the elements of the target array.
    *
    *  @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
    *    @inheritdoc
    *
    *    $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
    var i = start
    val end = start + spire.math.min(len, xs.length - start)
    while (i < end && this.hasNext) {
      xs(i) = this.next()
      i += 1
    }
    // TODO: return i - start so the caller knows how many values read?
  }

  /** Displays all elements of this $coll in a string using start, end, and
    *  separator strings.
    *
    *  @param start the starting string.
    *  @param sep   the separator string.
    *  @param end   the ending string.
    *  @return      a string representation of this $coll. The resulting string
    *               begins with the string `start` and ends with the string
    *               `end`. Inside, the string representations (w.r.t. the method
    *               `toString`) of all elements of this $coll are separated by
    *               the string `sep`.
    *
    *  @example  `List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
    */
  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  /** Displays all elements of this $coll in a string using a separator string.
    *
    *  @param sep   the separator string.
    *  @return      a string representation of this $coll. In the resulting string
    *               the string representations (w.r.t. the method `toString`)
    *               of all elements of this $coll are separated by the string `sep`.
    *
    *  @example  `List(1, 2, 3).mkString("|") = "1|2|3"`
    */
  def mkString(sep: String): String  = mkString("", sep, "")

  /** Displays all elements of this $coll in a string.
    *
    *  @return a string representation of this $coll. In the resulting string
    *          the string representations (w.r.t. the method `toString`)
    *          of all elements of this $coll follow each other without any
    *          separator string.
    */
  def mkString: String = mkString("")

  /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
    *  The written text begins with the string `start` and ends with the string `end`.
    *  Inside, the string representations (w.r.t. the method `toString`)
    *  of all elements of this $coll are separated by the string `sep`.
    *
    * Example:
    *
    * {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = new StringBuilder()
    *      b: StringBuilder =
    *
    *      scala> a.addString(b , "List(" , ", " , ")")
    *      res5: StringBuilder = List(1, 2, 3, 4)
    * }}}
    *
    *  @param  b    the string builder to which elements are appended.
    *  @param start the starting string.
    *  @param sep   the separator string.
    *  @param end   the ending string.
    *  @return      the string builder `b` to which elements were appended.
    */

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    var first = true

    b.append(start)
    for (x <- self) {
      if (first) {
        b append x
        first = false
      }
      else {
        b.append(sep)
        b.append(x)
      }
    }
    b.append(end)

    b
  }

  /** Appends all elements of this $coll to a string builder using a separator string.
    *  The written text consists of the string representations (w.r.t. the method `toString`)
    *  of all elements of this $coll, separated by the string `sep`.
    *
    * Example:
    *
    * {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = new StringBuilder()
    *      b: StringBuilder =
    *
    *      scala> a.addString(b, ", ")
    *      res0: StringBuilder = 1, 2, 3, 4
    * }}}
    *
    *  @param  b    the string builder to which elements are appended.
    *  @param sep   the separator string.
    *  @return      the string builder `b` to which elements were appended.
    */
  def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

  /** Appends all elements of this $coll to a string builder.
    *  The written text consists of the string representations (w.r.t. the method
    * `toString`) of all elements of this $coll without any separator string.
    *
    * Example:
    *
    * {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = new StringBuilder()
    *      b: StringBuilder =
    *
    *      scala> val h = a.addString(b)
    *      h: StringBuilder = 1234
    * }}}

    *  @param  b    the string builder to which elements are appended.
    *  @return      the string builder `b` to which elements were appended.
    */
  def addString(b: StringBuilder): StringBuilder = addString(b, "")

  /** Tests if another iterator produces the same values as this one.
    *
    *  $willNotTerminateInf
    *
    *  @param that  the other iterator
    *  @return      `true`, if both iterators produce the same elements in the same order, `false` otherwise.
    *
    *  @note        Reuse: $consumesTwoIterators
    */
  def sameElements(that: VectorReverseIterator[_]): Boolean = {
    while (hasNext && that.hasNext)
      if (next != that.next)
        return false

    !hasNext && !that.hasNext
  }

}
