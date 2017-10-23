package Cheetah.Immutable

import java.util.NoSuchElementException

import Cheetah.Immutable.Vector.empty

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Stream
import scala.collection.mutable.ArrayBuffer
import scala.collection.{GenIterable, GenMap, GenSeq, GenTraversable, immutable, mutable}
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag
import scala.{Vector => ScalaVector}

object Vector {

  def newBuilder[A : ClassTag]: VectorBuilder[A] = new VectorBuilder[A]()

  /** The generic builder that builds instances of $Coll
    * at arbitrary element types.
    */
  def genericBuilder[B : ClassTag]: VectorBuilder[B] = new VectorBuilder[B]()

  implicit def canBuildFrom[A : ClassTag]: CanBuildFrom[Vector[_], A, Vector[A]] =
    new CanBuildFrom[Vector[_], A, Vector[A]] {
      def apply: VectorBuilder[A] = new VectorBuilder[A]()

      override def apply(from: Vector[_]): VectorBuilder[A] = new VectorBuilder[A]()
    }
  @inline private[Immutable] final val compileAssertions = false

  def empty[A : ClassTag]: Vector[A] = new Vector[A](0)

  final lazy private[Immutable] val emptyTransientBlock: Array[AnyRef] = new Array[AnyRef](2)
}

final class Vector[+A: ClassTag](override private[Immutable] val endIndex: Int)
  extends VectorPointer[A@uncheckedVariance]
    with Serializable { self =>

  private[Immutable] var transient: Boolean = false

  // GenSeqLike

  def seq: Vector[A] = this

  /** Selects an element by its index in the type constructor Vector
    *
    * Example:
    *
    * {{{
    *    scala> val x = Vector(1, 2, 3, 4, 5)
    *    x: Vector[Int] = Vector(1, 2, 3, 4, 5)
    *
    *    scala> x(3)
    *    res1: Int = 4
    * }}}
    *
    * @param  index The index to select.
    * @return the element of this $coll at index `index`, where `0` indicates the first element.
    * @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    */
  def apply(index: Int): A = {
    val _focusStart: Int = this.focusStart
    if (_focusStart <= index && index < this.focusEnd) {
      val indexInFocus: Int = index - _focusStart
      getElem(indexInFocus, indexInFocus ^ this.focus)
    } else if (isDefinedAt(index)) {
      if (this.transient) {
        this.normalize(this.depth)
        this.transient = false
      }
      getElementFromRoot(index)
    } else {
      throw new IndexOutOfBoundsException(index.toString)
    }
  }

  /** The length/size of the $coll.
    *
    * $willNotTerminateInf
    *
    * Note: `xs.length` and `xs.size` yield the same result.
    *
    * @return the number of elements in this $coll.
    */
  def length: Int = endIndex

  def size: Int = endIndex

  /** Tests whether this $coll contains given index.
    *
    * The implementations of methods `apply` and `isDefinedAt` turn a `Seq[A]` into
    * a `PartialFunction[Int, A]`.
    *
    * @param    index the index to test
    * @return `true` if this $coll contains an element at position `idx`, `false` otherwise.
    */
  @inline def isDefinedAt(index: Int): Boolean = (index >= 0) && (index < this.endIndex)

  /** Computes length of longest segment whose elements all satisfy some predicate.
    *
    * $mayNotTerminateInf
    *
    * @param   p    the predicate used to test elements.
    * @param   from the index where the search starts.
    * @return the length of the longest segment of this $coll starting from index `from`
    *         such that every element of the segment satisfies the predicate `p`.
    */
  def segmentLength(p: A => Boolean, from: Int): Int = {
    if (from >= endIndex) 0 else {
      var i: Int = 0
      val forward: VectorIterator[A] = iterator(from, endIndex)
      while (forward.hasNext && p(forward.next())) i += 1
      i
    }
  }

  /** Returns the length of the longest prefix whose elements all satisfy some predicate.
    *
    * $mayNotTerminateInf
    *
    * @param   p the predicate used to test elements.
    * @return the length of the longest prefix of this $coll
    *         such that every element of the segment satisfies the predicate `p`.
    */
  def prefixLength(p: A => Boolean): Int = segmentLength(p, 0)

  /** Finds index of the first element satisfying some predicate after or at some start index.
    *
    * $mayNotTerminateInf
    *
    * @param   p    the predicate used to test elements.
    * @param   from the start index
    * @return the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
    *         or `-1`, if none exists.
    */
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i: Int = from
    val forward: VectorIterator[A] = iterator(from, endIndex)
    while (forward.hasNext) {
      if (p(forward.next())) return i
      i += 1
    }
    -1
  }

  /** Finds index of first element satisfying some predicate.
    *
    * $mayNotTerminateInf
    *
    * @param   p the predicate used to test elements.
    * @return the index of the first element of this $coll that satisfies the predicate `p`,
    *         or `-1`, if none exists.
    */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Finds index of first occurrence of some value in this $coll.
    *
    * @param   elem the element value to search for.
    * @tparam  B the type of the element `elem`.
    * @return the index of the first element of this $coll that is equal (as determined by `==`)
    *         to `elem`, or `-1`, if none exists.
    * @usecase def indexOf(elem: A): Int
    * @inheritdoc
    *
    * $mayNotTerminateInf
    *
    */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
    *
    * @param   elem the element value to search for.
    * @tparam  B the type of the element `elem`.
    * @param   from the start index
    * @return the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
    *         to `elem`, or `-1`, if none exists.
    * @usecase def indexOf(elem: A, from: Int): Int
    * @inheritdoc
    *
    * $mayNotTerminateInf
    *
    */
  def indexOf[B >: A](elem: B, from: Int): Int = indexWhere(elem == _, from)

  /** Finds index of last occurrence of some value in this $coll.
    *
    * @param   elem the element value to search for.
    * @tparam  B the type of the element `elem`.
    * @return the index of the last element of this $coll that is equal (as determined by `==`)
    *         to `elem`, or `-1`, if none exists.
    * @usecase def lastIndexOf(elem: A): Int
    * @inheritdoc
    *
    * $willNotTerminateInf
    *
    */
  def lastIndexOf[B >: A](elem: B): Int = lastIndexWhere(elem == _)

  /** Finds index of last occurrence of some value in this $coll before or at a given end index.
    *
    * @param   elem the element value to search for.
    * @param   end  the end index.
    * @tparam  B the type of the element `elem`.
    * @return the index `<= end` of the last element of this $coll that is equal (as determined by `==`)
    *         to `elem`, or `-1`, if none exists.
    * @usecase def lastIndexOf(elem: A, end: Int): Int
    * @inheritdoc
    */
  def lastIndexOf[B >: A](elem: B, end: Int): Int = lastIndexWhere(elem == _, end)

  /** Finds index of last element satisfying some predicate.
    *
    * $willNotTerminateInf
    *
    * @param   p the predicate used to test elements.
    * @return the index of the last element of this $coll that satisfies the predicate `p`,
    *         or `-1`, if none exists.
    */
  def lastIndexWhere(p: A => Boolean): Int = lastIndexWhere(p, endIndex)

  /** Finds index of last element satisfying some predicate before or at given end index.
    *
    * @param   p the predicate used to test elements.
    * @return the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
    *         or `-1`, if none exists.
    */
  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i: Int = endIndex - 1
    val reverse: VectorReverseIterator[A] = reverseiterator(0, end)
    while (reverse.hasNext && !p(reverse.next())) i -= 1
    i
  }

  /** Returns new $coll with elements within start to end in reversed order.
    *
    * $willNotTerminateInf
    *
    * @param start : Int
    * @param end   : Int
    * @return A new $coll with all elements of this $coll in reversed order.
    */
  def reverse(start: Int, end: Int): Vector[A] = {
    val reverse: VectorReverseIterator[A] = reverseiterator(start, end)
    val build: VectorBuilder[A] = newBuilder
    while (reverse.hasNext) build += reverse.next()
    build.result()
  }

  def reverse: Vector[A] = reverse(0, endIndex)

  /**
    * Builds a new collection by applying a function to all elements of this Vector type constructor and
    * collecting the results in reversed order.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `Vector[B]` resulting from applying the given function
    *         `f` to each element of this Vector and collecting the results in reversed order.
    * @usecase def reverseMap[B](f: A => B): Vector[B]
    * @inheritdoc
    *
    * $willNotTerminateInf
    *
    * Note: `xs.reverseMap(f)` is the same as `xs.reverse.map(f)` but might be more efficient.
    * @return a new Vector resulting from applying the given function
    *         `f` to each element of this Vector and collecting the results in reversed order.
    */
  def reverseMap[B : ClassTag](f: A => B): Vector[B] = {
    val reverse: VectorReverseIterator[A] = reverseiterator(0, endIndex)
    val build: VectorBuilder[B] = genericBuilder[B]
    while (reverse.hasNext) build += f(reverse.next())
    build.result()
  }

  /** Tests whether this $coll starts with the given sequence.
    *
    * @param  that the sequence to test
    * @return `true` if this collection has `that` as a prefix, `false` otherwise.
    */
  def startsWith[B](that: Vector[B]): Boolean = startsWith(that, 0)

  /** Tests whether this $coll contains the given sequence at a given index.
    *
    * '''Note''': If the both the receiver object `this` and the argument
    * `that` are infinite sequences this method may not terminate.
    *
    * @param  that   the sequence to test
    * @param  offset the index where the sequence is searched.
    * @return `true` if the sequence `that` is contained in this $coll at
    *         index `offset`, otherwise `false`.
    */
  def startsWith[B](that: Vector[B], offset: Int): Boolean = {
    if (offset >= this.endIndex) false
    else if ((this.endIndex - offset) != that.endIndex) false
    else {
      var i: Int = 0
      val thisforward: VectorIterator[A] = this.iterator(offset, this.endIndex)
      val thatforward: VectorIterator[B] = that.iterator(0, that.endIndex)
      while (thatforward.hasNext && thisforward.next() == thatforward.next()) i += 1
      i == that.endIndex
    }
  }

  /** Tests whether this $coll ends with the given sequence.
    * $willNotTerminateInf
    *
    * @param  that the sequence to test
    * @return `true` if this $coll has `that` as a  , `false` otherwise.
    */
  def endsWith[B](that: Vector[B]): Boolean = {
    if (this.endIndex < that.endIndex) false
    else {
      var i: Int = 0
      val thisreverse: VectorReverseIterator[A] = this.reverseiterator
      val thatreverse: VectorReverseIterator[B] = that.reverseiterator
      while (thatreverse.hasNext && thisreverse.next() == thatreverse.next()) i += 1
      i == that.endIndex
    }
  }

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
    *
    * @param  from     the index of the first replaced element
    * @param  patch    the replacement sequence
    * @param  replaced the number of elements to drop in the original $coll
    * @tparam B the element type of the returned $coll.
    * @return a new $coll consisting of all elements of this $coll
    *         except that `replaced` elements starting from `from` are replaced
    *         by `patch`.
    * @usecase def patch(from: Int, that: GenSeq[A], replaced: Int): $Coll[A]
    * @inheritdoc
    * @return a new $coll consisting of all elements of this $coll
    *         except that `replaced` elements starting from `from` are replaced
    *         by `patch`.
    */
  def patch[B >: A : ClassTag](from: Int, patch: Vector[B], replaced: Int): Vector[B] = {
    val vector: Vector[B] = new Vector[B](endIndex)
    vector.transient = this.transient
    vector.initWithFocusFrom(this.asInstanceOf[Vector[B]])

    var index: Int = {
      if (from > 0) {
        if (from < vector.focusStart || vector.focusEnd <= from || ((from - vector.focusStart) & ~31) != (vector.focus & ~31)) {
          if (!vector.isDefinedAt(from)) throw new IndexOutOfBoundsException(from.toString)
          vector.normalizeAndFocusOn(from)
        }
        from - ((from - vector.focusStart) & 31)
      } else {
        0
      }
    }

    // Pad the patch with enough elements so that blocks are aligned
    var i: Int = from - 1
    while (i >= index) {
      this (i) +: patch
      i -= 1
    }

    while (index < from + spire.math.min(replaced, patch.length)) {
      // Focus on the index that needs to be updated
      if (index < vector.focusStart || vector.focusEnd <= index || ((index - vector.focusStart) & ~31) != (vector.focus & ~31)) {
        if (!vector.isDefinedAt(index)) throw new IndexOutOfBoundsException(index.toString)
        vector.normalizeAndFocusOn(index)
      }
      vector.makeTransientIfNeeded()

      if ((from + spire.math.min(replaced, patch.length) - index) < 32) {
        // Replace only the subset of the Array if less than a block of elements are left to be updated

        val d0: Array[B] = copyOf(vector.display0.asInstanceOf[Leaf]).asInstanceOf[Array[B]]
        while (index < from + spire.math.min(replaced, patch.length)) {
          d0.update((index - vector.focusStart) & 31, patch(index - from))
          index += 1
        }
        vector.display0 = d0
      } else {
        // Replace by a copy of the whole Array when all the elements of the block need to be updated

        patch.normalizeAndFocusOn(index - from)
        patch.makeTransientIfNeeded()
        vector.display0 = copyOf(patch.display0.asInstanceOf[Leaf]).asInstanceOf[Array[B]]
        index += 32
      }
    }
    vector
  }

  /** A copy of this $coll with one single replaced element.
    *
    * @param  index the position of the replacement
    * @param  elem  the replacing element
    * @tparam B the element type of the returned $coll.
    * @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
    * @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    * @usecase def update(index: Int, elem: A): $Coll[A]
    * @inheritdoc
    * @return a copy of this $coll with the element at position `index` replaced by `elem`.
    */
  def update[B >: A : ClassTag](index: Int, elem: B): Vector[B] = {
    val vector: Vector[B] = new Vector[B](endIndex)
    vector.transient = this.transient
    vector.initWithFocusFrom(this.asInstanceOf[Vector[B]])

    if (index < focusStart || focusEnd <= index || ((index - focusStart) & ~31) != (focus & ~31)) {
      if (!vector.isDefinedAt(index)) throw new IndexOutOfBoundsException(index.toString)
      vector.normalizeAndFocusOn(index)
    }
    vector.makeTransientIfNeeded()

    val d0: Array[B] = copyOf(vector.display0.asInstanceOf[Leaf]).asInstanceOf[Array[B]]
    d0.update((index - vector.focusStart) & 31, elem)
    vector.display0 = d0
    vector.asInstanceOf[Vector[B]]
  }

  /** A copy of the $coll with an element prepended.
    *
    * @param  elem the prepended element
    * @tparam B the element type of the returned $coll.
    * @return a new collection of type `That` consisting of `elem` followed
    *         by all elements of this $coll.
    * @usecase def +:(elem: A): $Coll[A]
    * @inheritdoc
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the Colon goes on the Collection side.
    *
    * Also, the original $coll is not modified, so you will want to capture the result.
    *
    * Example:
    * {{{
    *      scala> val x = List(1)
    *      x: List[Int] = List(1)
    *
    *      scala> val y = 2 +: x
    *      y: List[Int] = List(2, 1)
    *
    *      scala> println(x)
    *      List(1)
    *    }}}
    * @return a new $coll consisting of `elem` followed
    *         by all elements of this $coll.
    */
  def +:[B >: A : ClassTag](elem: B): Vector[B] = {
    if (this.length != 0) {
      val vector = new Vector[B](this.endIndex + 1)
      vector.transient = this.transient
      vector.initWithFocusFrom(this.asInstanceOf[Vector[B]])
      vector.prepend(elem)
      vector.asInstanceOf[Vector[B]]
    } else {
      createSingletonVector(elem)
    }

  }

  /** A copy of this $coll with an element appended.
    *
    * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    *
    * @param  elem the appended element
    * @tparam B the element type of the returned $coll.
    * @return a new collection of type `That` consisting of
    *         all elements of this $coll followed by `elem`.
    * @usecase def :+(elem: A): $Coll[A]
    * @inheritdoc
    *
    * $willNotTerminateInf
    *
    * Example:
    * {{{
    *       scala> val a = List(1)
    *       a: List[Int] = List(1)
    *
    *       scala> val b = a :+ 2
    *       b: List[Int] = List(1, 2)
    *
    *       scala> println(a)
    *       List(1)
    *    }}}
    * @return a new $coll consisting of
    *         all elements of this $coll followed by `elem`.
    */
  def :+[B >: A : ClassTag](elem: B): Vector[B] = {
    if (this.endIndex != 0) {
      val resultVector: Vector[B] = new Vector[B](this.endIndex + 1)
      resultVector.transient = this.transient
      resultVector.initWithFocusFrom(this.asInstanceOf[Vector[B]])
      resultVector.append(elem, this.endIndex)
      resultVector.asInstanceOf[Vector[B]]
    } else {
      createSingletonVector(elem)
    }
  }

  /** A copy of this $coll with an element value appended until a given target length is reached.
    *
    * @param   length the target length
    * @param   elem   the padding value
    * @tparam B the element type of the returned $coll.
    * @return a new collection of type `That` consisting of
    *         all elements of this $coll followed by the minimal number of occurrences of `elem` so
    *         that the resulting collection has a length of at least `len`.
    * @usecase def padTo(len: Int, elem: A): $Coll[A]
    * @inheritdoc
    * @return a new $coll consisting of
    *         all elements of this $coll followed by the minimal number of occurrences of `elem` so
    *         that the resulting $coll has a length of at least `len`.
    */
  def padTo[B >: A : ClassTag](length: Int, elem: B): Vector[B] = {
    val build: VectorBuilder[B] = genericBuilder[B]
    var index = length - this.length
    build ++= this
    while (index > 0) {
      build += elem
      index -= 1
    }
    build.result()
  }

  /** Tests whether every element of this $coll relates to the
    * corresponding element of another sequence by satisfying a test predicate.
    *
    * @param   that the other sequence
    * @param   p    the test predicate, which relates elements from both sequences
    * @tparam  B the type of the elements of `that`
    * @return `true` if both sequences have the same length and
    *         `p(x, y)` is `true` for all corresponding elements `x` of this $coll
    *         and `y` of `that`, otherwise `false`.
    */
  def corresponds[B](that: Vector[B])(p: (A, B) => Boolean): Boolean = {
    val thisforward: VectorIterator[A] = this.iterator
    val thatforward = that.iterator
    if (this.length == that.length) { // If both vectors have the same length
      while (thisforward.hasNext) { // If every corresponding element satisfies the predicate
        if (!p(thisforward.next(), thatforward.next())) return false
      }
      true
    } else {
      false
    }
  }

  /** Produces a new sequence which contains all elements of this $coll and also all elements of
    * a given sequence. `xs union ys`  is equivalent to `xs ++ ys`.
    *
    * @param that the sequence to add.
    * @tparam B the element type of the returned $coll.
    * @return a new collection of type `That` which contains all elements of this $coll
    *         followed by all elements of `that`.
    * @usecase def union(that: GenSeq[A]): $Coll[A]
    * @inheritdoc
    *
    * Another way to express this
    * is that `xs union ys` computes the order-preserving multi-set union of `xs` and `ys`.
    * `union` is hence a counter-part of `diff` and `intersect` which also work on multi-sets.
    *
    * $willNotTerminateInf
    * @return a new $coll which contains all elements of this $coll
    *         followed by all elements of `that`.
    */
  def union[B >: A : ClassTag](that: Vector[B]): Vector[B] = this ++ that

  /** Computes the multiset difference between this $coll and another sequence.
    *
    * @param that the sequence of elements to remove
    * @tparam B the element type of the returned $coll.
    * @return a new collection of type `That` which contains all elements of this $coll
    *         except some of occurrences of elements that also appear in `that`.
    *         If an element value `x` appears
    *         ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
    *         part of the result, but any following occurrences will.
    * @usecase def diff(that: GenSeq[A]): $Coll[A]
    * @inheritdoc
    *
    * $willNotTerminateInf
    * @return a new $coll which contains all elements of this $coll
    *         except some of occurrences of elements that also appear in `that`.
    *         If an element value `x` appears
    *         ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
    *         part of the result, but any following occurrences will.
    */
  def diff[B >: A](that: Vector[B]): Vector[A] = {
    val occurs: mutable.Map[B, Int] = that.occurrences
    val build: VectorBuilder[A] = newBuilder
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: A = forward.next()
      val count: Int = occurs(x)
      if (count == 0) {
        build += x
      } else {
        occurs(x) = count - 1
      }
    }
    build.result()
  }

  /** Computes the multiset intersection between this $coll and another sequence.
    *
    * @param that the sequence of elements to intersect with.
    * @tparam B the element type of the returned $coll.
    * @return a new collection of type `That` which contains all elements of this $coll
    *         which also appear in `that`.
    *         If an element value `x` appears
    *         ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
    *         in the result, but any following occurrences will be omitted.
    * @usecase def intersect(that: GenSeq[A]): $Coll[A]
    * @inheritdoc
    *
    * $mayNotTerminateInf
    * @return a new $coll which contains all elements of this $coll
    *         which also appear in `that`.
    *         If an element value `x` appears
    *         ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
    *         in the result, but any following occurrences will be omitted.
    */
  def intersect[B >: A](that: Vector[B]): Vector[A] = {
    val occurs: mutable.Map[B, Int] = that.occurrences
    val build: VectorBuilder[A] = newBuilder
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: A = forward.next()
      val count: Int = occurs(x)
      if (count != 0) {
        build += x
        occurs(x) = count - 1
      }
    }
    build.result()
  }

  /** Builds a new $coll from this $coll without any duplicate elements.
    * $willNotTerminateInf
    *
    * @return A new $coll which contains the first occurrence of every element of this $coll.
    */
  def distinct: Vector[A] = {
    val occurrence: mutable.HashMap[A, Int] = new mutable.HashMap[A, Int] {
      override def default(k: A): Int = 0
    }
    val build: VectorBuilder[A] = newBuilder
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: A = forward.next()
      occurrence(x) += 1
      if (occurrence(x) == 1) {
        build += x
      }
    }
    build.result()
  }

  /** Hashcodes for $Coll produce a value from the hashcodes of all the
    * elements of the $coll.
    */
  override def hashCode(): Int = scala.util.hashing.MurmurHash3.hashCode()

  /** The equals method for arbitrary sequences. Compares this sequence to
    * some other object.
    *
    * @param    that The object to compare the sequence to
    * @return `true` if `that` is a sequence that has the same elements as
    *         this sequence in the same order, `false` otherwise
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Vector[_] => (this corresponds that) ((x, y) => x == y)
      case _ => false
    }
  }

  // GenericTraversableTemplate

  /** Applies a function `f` to all elements of this $coll.
    *
    * @param  f the function that is applied for its side-effect to every element.
    *           The result of function `f` is discarded.
    * @tparam  B the type parameter describing the result of function `f`.
    *            This result will always be ignored. Typically `U` is `Unit`,
    *            but this is not necessary.
    * @usecase def foreach(f: A => Unit): Unit
    * @inheritdoc
    *
    * Note: this method underlies the implementation of most other bulk operations.
    * It's important to implement this method in an efficient way.
    *
    */
  def foreach[B](f: A => B): Unit = {
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) f(forward.next())
  }

  /** Selects the first element of this $coll.
    *
    * @return the first element of this $coll.
    * @throws NoSuchElementException if the $coll is empty.
    */
  def head: A = {
    if (this.endIndex != 0) {
      this.apply(0)
    }
    else {
      throw new UnsupportedOperationException("empty.head")
    }
  }

  /** Tests whether this $coll is empty.
    *
    * @return `true` if the $coll contain no elements, `false` otherwise.
    */
  def isEmpty: Boolean = this.endIndex == 0

  /** Tests whether the $coll is not empty.
    *
    * @return `true` if the $coll contains at least one element, `false` otherwise.
    */
  def nonEmpty: Boolean = !isEmpty

  //  /** The factory companion object that builds instances of class $Coll.
  //    * (or its `Iterable` superclass where class $Coll is not a `Seq`.)
  //    */
  //  def companion: GenericCompanion[Vector] = new GenericCompanion[Vector] {
  //    override def newBuilder[A]: VectorBuilder[A] = new VectorBuilder[A]
  //  }

  /** The builder that builds instances of type $Coll[A]
    */
  protected[this] def newBuilder: VectorBuilder[A] = new VectorBuilder[A]

  /** The generic builder that builds instances of $Coll
    * at arbitrary element types.
    */
  def genericBuilder[B: ClassTag]: VectorBuilder[B] = new VectorBuilder[B]

  /** Returns a $coll formed from this $coll and another iterable collection
    * by combining corresponding elements in pairs.
    * If one of the two collections is longer than the other, its remaining elements are ignored.
    *
    * @param   B The iterable providing the second half of each result pair
    * @tparam  A the type of the first half of the returned pairs (this is always a supertype
    *            of the collection's element type `A`).
    * @tparam  B the type of the second half of the returned pairs
    * @return a new collection of type `That` containing pairs consisting of
    *         corresponding elements of this $coll and `that`. The length
    *         of the returned collection is the minimum of the lengths of this $coll and `that`.
    * @usecase def zip[B](that: Vector[B]): $Coll[(A, B)]
    * @inheritdoc
    *
    * $orderDependent
    * @param   that The iterable providing the second half of each result pair
    * @tparam  B the type of the second half of the returned pairs
    * @return a new $coll containing pairs consisting of
    *         corresponding elements of this $coll and `that`. The length
    *         of the returned collection is the minimum of the lengths of this $coll and `that`.
    */
  def zip[B: ClassTag](that: Vector[B]): Vector[(A, B)] = {
    val build: VectorBuilder[(A, B)] = genericBuilder[(A, B)]
    val thisforward: VectorIterator[A] = this.iterator
    val thatforward: VectorIterator[B] = that.iterator
    while (thisforward.hasNext && thatforward.hasNext) build += Tuple2(thisforward.next(), thatforward.next())
    build.result()
  }

  /** Zips this $coll with its indices.
    *
    * @tparam  B the type of the first half of the returned pairs (this is always a supertype
    *            of the collection's element type `A`).
    * @tparam    [(B , Int)] the class of the returned collection. Where possible, `That` is
    *            the same class as the current collection class `Repr`, but this
    *            depends on the element type `(A1, Int)` being admissible for that class,
    *            which means that an implicit instance of type `CanBuildFrom[Repr, (A1, Int), That]`.
    *            is found.
    * @return A new collection of type `That` containing pairs consisting of all elements of this
    *         $coll paired with their index. Indices start at `0`.
    * @usecase def zipWithIndex: $Coll[(A, Int)]
    * @inheritdoc
    *
    * $orderDependent
    * @return A new $coll containing pairs consisting of all elements of this
    *         $coll paired with their index. Indices start at `0`.
    * @example
    * `List("a", "b", "c").zipWithIndex = List(("a", 0), ("b", 1), ("c", 2))`
    *
    */
  def zipWithIndex[B >: A]: Vector[(B, Int)] = {
    val build: VectorBuilder[(B, Int)] = genericBuilder[(B, Int)]
    val forward: VectorIterator[A] = this.iterator
    var index: Int = 0
    while (forward.hasNext) {
      build += Tuple2(forward.next(), index)
      index += 1
    }
    build.result()
  }

  /** Returns a $coll formed from this $coll and another iterable collection
    * by combining corresponding elements in pairs.
    * If one of the two collections is shorter than the other,
    * placeholder elements are used to extend the shorter collection to the length of the longer.
    *
    * @param that     the iterable providing the second half of each result pair
    * @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
    * @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
    * @return a new collection of type `That` containing pairs consisting of
    *         corresponding elements of this $coll and `that`. The length
    *         of the returned collection is the maximum of the lengths of this $coll and `that`.
    *         If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
    *         If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
    * @usecase def zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): $Coll[(A, B)]
    * @inheritdoc
    *
    * $orderDependent
    * @param   that   The iterable providing the second half of each result pair
    * @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
    * @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
    * @tparam  B the type of the second half of the returned pairs
    * @return a new $coll containing pairs consisting of
    *         corresponding elements of this $coll and `that`. The length
    *         of the returned collection is the maximum of the lengths of this $coll and `that`.
    *         If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
    *         If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
    */
  def zipAll[B, A1 >: A](that: Vector[B], thisElem: A1, thatElem: B): Vector[(A1, B)] = {
    val build: VectorBuilder[(A1, B)] = genericBuilder[(A1, B)]
    val thisforward: VectorIterator[A] = this.iterator
    val thatforward: VectorIterator[B] = that.iterator
    while (thisforward.hasNext && thatforward.hasNext) build += Tuple2(thisforward.next(), thatforward.next())
    while (thisforward.hasNext) build += Tuple2(thisforward.next(), thatElem)
    while (thatforward.hasNext) build += Tuple2(thisElem, thatforward.next())
    build.result()
  }

  /** Converts this $coll of pairs into two collections of the first and second
    * half of each pair.
    *
    * {{{
    *    val xs = $Coll(
    *               (1, "one"),
    *               (2, "two"),
    *               (3, "three")).unzip
    *    // xs == ($Coll(1, 2, 3),
    *    //        $Coll(one, two, three))
    * }}}
    *
    * @tparam A1 the type of the first half of the element pairs
    * @tparam A2 the type of the second half of the element pairs
    * @param asPair an implicit conversion which asserts that the element type
    *               of this $coll is a pair.
    * @return a pair of ${coll}s, containing the first, respectively second
    *         half of each element pair of this $coll.
    */
  def unzip[A1 : ClassTag, A2 : ClassTag](implicit asPair: A => (A1, A2)): (Vector[A1], Vector[A2]) = {
    val build1: VectorBuilder[A1] = genericBuilder[A1]
    val build2: VectorBuilder[A2] = genericBuilder[A2]
    this.foreach(xy => {
      val (x, y) = asPair(xy)
      build1 += x
      build2 += y
    })
    (build1.result(), build2.result())
  }

  /** Converts this $coll of triples into three collections of the first, second,
    * and third element of each triple.
    *
    * {{{
    *    val xs = $Coll(
    *               (1, "one", '1'),
    *               (2, "two", '2'),
    *               (3, "three", '3')).unzip3
    *    // xs == ($Coll(1, 2, 3),
    *    //        $Coll(one, two, three),
    *    //        $Coll(1, 2, 3))
    * }}}
    *
    * @tparam A1 the type of the first member of the element triples
    * @tparam A2 the type of the second member of the element triples
    * @tparam A3 the type of the third member of the element triples
    * @param asTriple an implicit conversion which asserts that the element type
    *                 of this $coll is a triple.
    * @return a triple of ${coll}s, containing the first, second, respectively
    *         third member of each element triple of this $coll.
    */
  def unzip3[A1 : ClassTag, A2 : ClassTag, A3 : ClassTag](implicit asTriple: A => (A1, A2, A3)): (Vector[A1], Vector[A2], Vector[A3]) = {
    val build1: VectorBuilder[A1] = genericBuilder[A1]
    val build2: VectorBuilder[A2] = genericBuilder[A2]
    val build3: VectorBuilder[A3] = genericBuilder[A3]

    this.foreach(xyz => {
      val (x, y, z) = asTriple(xyz)
      build1 += x
      build2 += y
      build3 += z
    })
    (build1.result(), build2.result(), build3.result())
  }

  /** Converts this $coll of traversable collections into
    * a $coll formed by the elements of these traversable
    * collections.
    *
    * @tparam B the type of the elements of each traversable collection.
    * @return a new $coll resulting from concatenating all element ${coll}s.
    * @usecase def flatten[B]: $Coll[B]
    * @inheritdoc
    *
    * The resulting collection's type will be guided by the
    * static type of $coll. For example:
    *
    * {{{
    *    val xs = List(
    *               Set(1, 2, 3),
    *               Set(1, 2, 3)
    *             ).flatten
    *    // xs == List(1, 2, 3, 1, 2, 3)
    *
    *    val ys = Set(
    *               List(1, 2, 3),
    *               List(3, 2, 1)
    *             ).flatten
    *    // ys == Set(1, 2, 3)
    *    }}}
    */
  def flatten[B : ClassTag](implicit asVector: A => Vector[B]): Vector[B] = {
    val build: VectorBuilder[B] = genericBuilder[B]
    this.foreach(xs => build ++= asVector(xs))
    build.result()
  }

  /** Transposes this $coll of traversable collections into
    * a $coll of ${coll}s.
    *
    * The resulting collection's type will be guided by the
    * static type of $coll. For example:
    *
    * {{{
    *    val xs = List(
    *               Set(1, 2, 3),
    *               Set(4, 5, 6)).transpose
    *    // xs == List(
    *    //         List(1, 4),
    *    //         List(2, 5),
    *    //         List(3, 6))
    *
    *    val ys = Vector(
    *               List(1, 2, 3),
    *               List(4, 5, 6)).transpose
    *    // ys == Vector(
    *    //         Vector(1, 4),
    *    //         Vector(2, 5),
    *    //         Vector(3, 6))
    * }}}
    *
    * @tparam B the type of the elements of each traversable collection.
    * @return a two-dimensional $coll of ${coll}s which has as ''n''th row
    *         the ''n''th column of this $coll.
    * @throws IllegalArgumentException if all collections in this $coll
    *                                  are not of the same size.
    */
  /*
  def transpose[B](implicit asVector: A => Vector[B]): Vector[Vector[B]@uncheckedVariance] = {

    if (isEmpty) return genericBuilder[Vector[B]].result()

    def fail: Nothing = throw new IllegalArgumentException("transpose requires all collections have the same size")

    val headSize: Int = asVector(this.head).size
    val bs: IndexedSeq[VectorBuilder[B]] = IndexedSeq.fill(headSize)(genericBuilder[B])
    this.foreach(xs => {
      var i: Int = 0
      asVector(xs).foreach(x => {
        if (i >= headSize) fail
        bs(i) += x
        i += 1
      })
      if (i != headSize) fail
    })
    val build: VectorBuilder[Vector[B]] = genericBuilder[Vector[B]]
    bs.foreach(b => build += b.result)
    build.result()
  }
*/

  // GenIterableLike

  /** Constructs an Iterator for the Vector type constructor
    *
    * @param start Int
    * @param end   Int
    * @return And iterator from start to end
    */

  final def iterator(start: Int, end: Int): VectorIterator[A] = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }

    val iterator: VectorIterator[A] = new VectorIterator[A](start, end)
    iterator.initIteratorFrom(this)
    iterator
  }

  final def iterator: VectorIterator[A] = iterator(0, endIndex)

  /** Constructs the Reverse Iterator for the Vector type constructor
    *
    * @param start Int
    * @param end   Int
    * @return And iterator from start to end
    */

  final def reverseiterator(start: Int, end: Int): VectorReverseIterator[A] = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }
    val reverseiterator: VectorReverseIterator[A] = new VectorReverseIterator[A](start, end)
    reverseiterator.initIteratorFrom(this)
    reverseiterator
  }

  final def reverseiterator: VectorReverseIterator[A] = reverseiterator(0, endIndex)

  /** Checks if the other iterable collection contains the same elements in the same order as this $coll.
    *
    * @param that the collection to compare with.
    * @tparam B the type of the elements of collection `that`.
    * @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
    * @usecase def sameElements(that: GenIterable[A]): Boolean
    * @inheritdoc
    *
    * $orderDependent
    * $willNotTerminateInf
    * @param that the collection to compare with.
    * @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
    */
  def sameElements[B >: A](that: Vector[B]): Boolean = this.corresponds(that)((x, y) => x == y)

  // GenTraversableLike

  /** Optionally selects the first element.
    * $orderDependent
    *
    * @return the first element of this $coll if it is nonempty,
    *         `None` if it is empty.
    */
  final def headOption: Option[A] = if (isEmpty) None else Some(this.head)

  /** Tests whether this $coll can be repeatedly traversed.
    *
    * @return `true`
    */
  final def isTraversableAgain: Boolean = true

  /** Selects all elements except the first.
    * $orderDependent
    *
    * @return a $coll consisting of all elements of this $coll
    *         except the first one.
    * @throws UnsupportedOperationException if the $coll is empty.
    */
  final def tail: Vector[A] = this.drop(1)

  /** Selects the last element.
    * $orderDependent
    *
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  final def last: A = if (this.isEmpty) throw new NoSuchElementException else this (this.length - 1)

  /** Optionally selects the last element.
    * $orderDependent
    *
    * @return the last element of this $coll$ if it is nonempty,
    *         `None` if it is empty.
    */
  final def lastOption: Option[A] = if (this.isEmpty) None else Some(this (this.length - 1))

  /** Selects all elements except the last.
    * $orderDependent
    *
    * @return a $coll consisting of all elements of this $coll
    *         except the last one.
    * @throws UnsupportedOperationException if the $coll is empty.
    */
  final def init: Vector[A] = take(this.length - 1)

  /** Computes a prefix scan of the elements of the collection.
    *
    * Note: The neutral element `z` may be applied more than once.
    *
    * @tparam B element type of the resulting collection
    * @param z  neutral element for the operator `op`
    * @param op the associative operator for the scan
    * @return a new $coll containing the prefix scan of the elements in this $coll
    */
  def scan[B >: A : ClassTag](z: B)(op: (B, B) => B): Vector[B] = scanLeft(z)(op) //TODO Maybe use a recursive method?

  /** Produces a collection containing cumulative results of applying the
    * operator going left to right.
    *
    * $willNotTerminateInf
    * $orderDependent
    *
    * @tparam B the type of the elements in the resulting collection
    * @param z  the initial value
    * @param op the binary operator applied to the intermediate result and the element
    * @return collection with intermediate results
    */
  def scanLeft[B : ClassTag](z: B)(op: (B, A) => B): Vector[B] = {
    val build: VectorBuilder[B] = genericBuilder[B]
    val forward: VectorIterator[A] = this.iterator
    var acc: B = z
    while (forward.hasNext) {
      acc = op(acc, forward.next())
      build += acc
    }
    build.result()
  }

  /** Produces a collection containing cumulative results of applying the operator going right to left.
    * The head of the collection is the last cumulative result.
    * $willNotTerminateInf
    * $orderDependent
    *
    * Example:
    * {{{
    *    Vector(1, 2, 3, 4).scanRight(0)(_ + _) == Vector(0, 4, 7, 9, 10)
    * }}}
    *
    * @tparam B    the type of the elements in the resulting collection
    * @tparam That the actual type of the resulting collection
    * @param z  the initial value
    * @param op the binary operator applied to the intermediate result and the element
    * @return collection with intermediate results
    */
  def scanRight[B : ClassTag](z: B)(op: (A, B) => B): Vector[B] = {
    val build: VectorBuilder[B] = genericBuilder[B]
    val backward: VectorReverseIterator[A] = this.reverseiterator
    var acc: B = z
    while (backward.hasNext) {
      acc = op(backward.next(), acc)
      build += acc
    }
    build.result()
  }

  /** Builds a new collection by applying a function to all elements of this $coll.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `That` resulting from applying the given function
    *         `f` to each element of this $coll and collecting the results.
    * @usecase def map[B](f: A => B): $Coll[B]
    * @inheritdoc
    * @return a new $coll resulting from applying the given function
    *         `f` to each element of this $coll and collecting the results.
    */
  def hashedmap[B : ClassTag](f: A => B): Vector[B] = {
    val value: mutable.HashMap[A, B] = new mutable.HashMap[A, B] {
      override def default(k: A): B = empty.asInstanceOf[B]
    }
    val build: VectorBuilder[B] = genericBuilder[B]
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      // TODO HashedMap map: Check if it actually helps,
      // or memory is a problem, if so we could use a zero-allocation hashing algorithm
      val x: A = forward.next()
      if(value(x) == null) value(x) = f(x)
      build += value(x)
    }
    build.result()
  }


  def map[B : ClassTag](f: A => B): Vector[B] = {
    val build: VectorBuilder[B] = genericBuilder[B]
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) build += f(forward.next())
    build.result()
  }

  /** Builds a new collection by applying a partial function to all elements of this $coll
    * on which the function is defined.
    *
    * @param pf the partial function which filters and maps the $coll.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `That` resulting from applying the partial function
    *         `pf` to each element on which it is defined and collecting the results.
    *         The order of the elements is preserved.
    * @usecase def collect[B](pf: PartialFunction[A, B]): $Coll[B]
    * @inheritdoc
    *
    * $collectExample
    * @return a new $coll resulting from applying the given partial function
    *         `pf` to each element on which it is defined and collecting the results.
    *         The order of the elements is preserved.
    */
  def collect[B: ClassTag](pf: PartialFunction[A, B]): Vector[B] = {
    val build: VectorBuilder[B] = genericBuilder[B]
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: A = forward.next()
      if (pf.isDefinedAt(x)) build += pf(x)
    }
    build.result()
  }

  /** Builds a new collection by applying a function to all elements of this $coll
    * and using the elements of the resulting collections.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `That` resulting from applying the given collection-valued function
    *         `f` to each element of this $coll and concatenating the results.
    * @usecase def flatMap[B](f: A => GenTraversableOnce[B]): $Coll[B]
    * @inheritdoc
    *
    * For example:
    *
    * {{{
    *      def getWords(lines: Seq[String]): Seq[String] = lines flatMap (line => line split "\\W+")
    *    }}}
    *
    * The type of the resulting collection is guided by the static type of $coll. This might
    * cause unexpected results sometimes. For example:
    *
    * {{{
    *      // lettersOf will return a Seq[Char] of likely repeated letters, instead of a Set
    *      def lettersOf(words: Seq[String]) = words flatMap (word => word.toSet)
    *
    *      // lettersOf will return a Set[Char], not a Seq
    *      def lettersOf(words: Seq[String]) = words.toSet flatMap (word => word.toSeq)
    *
    *      // xs will be an Iterable[Int]
    *      val xs = Map("a" -> List(11,111), "b" -> List(22,222)).flatMap(_._2)
    *
    *      // ys will be a Map[Int, Int]
    *      val ys = Map("a" -> List(1 -> 11,1 -> 111), "b" -> List(2 -> 22,2 -> 222)).flatMap(_._2)
    *    }}}
    * @return a new $coll resulting from applying the given collection-valued function
    *         `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[B: ClassTag](f: A => Vector[B]): Vector[B] = {
    val build: VectorBuilder[B] = genericBuilder[B]
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) build ++= f(forward.next())
    build.result()
  }

  def hashedflatMap[B : ClassTag](f: A => Vector[B]): Vector[B] = {
    val value: mutable.HashMap[A, Vector[B]] = new mutable.HashMap[A, Vector[B]] {
      override def default(k: A): Vector[B] = empty.asInstanceOf[Vector[B]]
    }
    val build: VectorBuilder[B] = genericBuilder[B]
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      // TODO HashedflatMap map: Check if it actually helps,
      // or memory is a problem, if so we could use a zero-allocation hashing algorithm
      val x: A = forward.next()
      if(value(x) == null) value(x) = f(x)
      build ++= value(x)
    }
    build.result()
  }

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    * right hand operand. The element type of the $coll is the most specific superclass encompassing
    * the element types of the two operands.
    *
    * @param that the traversable to append.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `That` which contains all elements
    *         of this $coll followed by all elements of `that`.
    */
  def ++[B >: A : ClassTag](that: Vector[B]): Vector[B] = {
    if (that.isEmpty) this.asInstanceOf[Vector[B]]
    else if (this.length == 0) {
      that.asInstanceOf[Vector[B]]
    } else {
      val vector: Vector[B] = new Vector[B](this.length + that.length)
      vector.initWithFocusFrom(this.asInstanceOf[Vector[B]])
      vector.transient = this.transient
      vector.concatenate(this.length, that)
      vector.asInstanceOf[Vector[B]]
    }
  }

  /** Selects all elements of this $coll which satisfy a predicate.
    *
    * @param p the predicate used to test elements.
    * @return a new $coll consisting of all elements of this $coll that satisfy the given
    *         predicate `p`. Their order is preserved.
    */
  def filter(p: A => Boolean): Vector[A] = { // TODO Is it better to implement it via hashmaps
    val build: VectorBuilder[A] = newBuilder
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: A = forward.next()
      if (p(x)) build += x
    }
    build.result()
  }

  /** Selects all elements of this $coll which do not satisfy a predicate.
    *
    * @param p the predicate used to test elements.
    * @return a new $coll consisting of all elements of this $coll that do not satisfy the given
    *         predicate `p`. Their order is preserved.
    */
  def filterNot(p: A => Boolean): Vector[A] = this.filter(x => !p(x))

  /** Partitions this $coll in two ${coll}s according to a predicate.
    *
    * @param p the predicate on which to partition.
    * @return a pair of ${coll}s: the first $coll consists of all elements that
    *         satisfy the predicate `p` and the second $coll consists of all elements
    *         that don't. The relative order of the elements in the resulting ${coll}s
    *         may not be preserved.
    */
  def partition(p: A => Boolean): (Vector[A], Vector[A]) = {
    val build1: VectorBuilder[A] = newBuilder
    val build2: VectorBuilder[A] = newBuilder
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: A = forward.next()
      if (p(x)) build1 += x else build2 += x
    }
    (build1.result(), build2.result())
  }

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    *
    * Note: this method is not re-implemented by views. This means
    * when applied to a view it will always force the view and
    * return a new $coll.
    *
    * @param f the discriminator function.
    * @tparam K the type of keys returned by the discriminator function.
    * @return A map from keys to ${coll}s such that the following invariant holds:
    *         {{{
    *                                            (xs groupBy f)(k) = xs filter (x => f(x) == k)
    *         }}}
    *         That is, every key `k` is bound to a $coll of those elements `x`
    *         for which `f(x)` equals `k`.
    *
    */
  def groupBy[B >: A : ClassTag, K] (f: B => K): mutable.Map[K, Vector[B]] = {
    val group: mutable.HashMap[K, Vector[B]] = new mutable.HashMap[K, Vector[B]] {
      override def default(k: K) = new Vector[B](0)
    }
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: B = forward.next()
      group(f(x)) :+ x
    }
    group
  }

  /** Selects first ''n'' elements.
    * $orderDependent
    *
    * @param  n the number of elements to take from this $coll.
    * @return a $coll consisting only of the first `n` elements of this $coll,
    *         or else the whole $coll, if it has less than `n` elements.
    */
  def take(n: Int): Vector[A] = {
    if (n <= 0) empty
    else if (n < endIndex)
      takeFront0(n)
    else
      this
  }

  /** Selects all elements except first ''n'' ones.
    * $orderDependent
    *
    * @param  n the number of elements to drop from this $coll.
    * @return a $coll consisting of all elements of this $coll except the first `n` ones, or else the
    *         empty $coll, if this $coll has less than `n` elements.
    */
  def drop(n: Int): Vector[A] = {
    if (n <= 0) this
    else if (n < endIndex)
      dropFront0(n)
    else
      empty
  }

  /** Selects an interval of elements.  The returned collection is made up
    * of all elements `x` which satisfy the invariant:
    * {{{
    *    from <= indexOf(x) < until
    * }}}
    * $orderDependent
    *
    * @param from  the lowest index to include from this $coll.
    * @param until the lowest index to EXCLUDE from this $coll.
    * @return a $coll containing the elements greater than or equal to
    *         index `from` extending up to (but not including) index `until`
    *         of this $coll.
    */
  def slice(from: Int, until: Int): Vector[A] = take(until).drop(from)

  /** Splits this $coll into two at a given position.
    * Note: `c splitAt n` is equivalent to (but possibly more efficient than)
    * `(c take n, c drop n)`.
    * $orderDependent
    *
    * @param n the position at which to split.
    * @return a pair of ${coll}s consisting of the first `n`
    *         elements of this $coll, and the other elements.
    */
  def splitAt(n: Int): (Vector[A], Vector[A]) = (this take n, this drop n)

  /** Takes longest prefix of elements that satisfy a predicate.
    * $orderDependent
    *
    * @param   p The predicate used to test elements.
    * @return the longest prefix of this $coll whose elements all satisfy
    *         the predicate `p`.
    */
  def takeWhile(p: A => Boolean): Vector[A] = take(prefixLength(p))

  /** Drops longest prefix of elements that satisfy a predicate.
    * $orderDependent
    *
    * @param   p The predicate used to test elements.
    * @return the longest suffix of this $coll whose first element
    *         does not satisfy the predicate `p`.
    */
  def dropWhile(p: A => Boolean): Vector[A] = drop(prefixLength(p))

  /** Splits this $coll into a prefix/suffix pair according to a predicate.
    *
    * Note: `c span p`  is equivalent to (but possibly more efficient than)
    * `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
    * predicate `p` does not cause any side-effects.
    * $orderDependent
    *
    * @param p the test predicate
    * @return a pair consisting of the longest prefix of this $coll whose
    *         elements all satisfy `p`, and the rest of this $coll.
    */
  def span(p: A => Boolean): (Vector[A], Vector[A]) = (this takeWhile p, this dropWhile p)

  /** Defines the prefix of this object's `toString` representation.
    *
    * @return a string representation which starts the result of `toString`
    *         applied to this $coll. By default the string prefix is the
    *         simple name of the collection class $coll.
    */
  def stringPrefix: String = "Vector"


  // Parallelizable

  /** Returns a parallel implementation of this collection.
    *
    * For most collection types, this method creates a new parallel collection by copying
    * all the elements. For these collection, `par` takes linear time. Mutable collections
    * in this category do not produce a mutable parallel collection that has the same
    * underlying dataset, so changes in one collection will not be reflected in the other one.
    *
    * Specific collections (e.g. `ParArray` or `mutable.ParHashMap`) override this default
    * behaviour by creating a parallel collection which shares the same underlying dataset.
    * For these collections, `par` takes constant or sublinear time.
    *
    * All parallel collections return a reference to themselves.
    *
    * @return a parallel implementation of this collection
    */
  /*  def par: ParRepr = {
      val cb = parCombiner
      for (x <- seq) cb += x
      cb.result()
    }*/

  /** The default `par` implementation uses the combiner provided by this method
    * to create a new parallel collection.
    *
    * @return a combiner for the parallel collection of type `ParRepr`
    */
  /*
    protected[this] def parCombiner: Combiner[A, ParRepr]
  */

  // GenTraversableOnce

  /** Tests whether this $coll is known to have a finite size.
    * All strict collections are known to have finite size. For a non-strict
    * collection such as `Stream`, the predicate returns `'''true'''` if all
    * elements have been computed. It returns `'''false'''` if the stream is
    * not yet evaluated to the end. Non-empty Iterators usually return
    * `'''false'''` even if they were created from a collection with a known
    * finite size.
    *
    * Note: many collection methods will not work on collections of infinite sizes.
    * The typical failure mode is an infinite loop. These methods always attempt a
    * traversal without checking first that `hasDefiniteSize` returns `'''true'''`.
    * However, checking `hasDefiniteSize` can provide an assurance that size is
    * well-defined and non-termination is not a concern.
    *
    * @return `'''true'''` if this collection is known to have finite size,
    *         `'''false'''` otherwise.
    */
  def hasDefiniteSize: Boolean = true

  /** The size of this $coll, if it can be cheaply computed
    *
    * @return the number of elements in this $coll, or -1 if the size cannot be determined cheaply
    */
  protected[Immutable] def sizeHintIfCheap: Int = this.length

  /** Reduces the elements of this $coll using the specified associative binary operator.
    *
    * $undefinedorder
    *
    * @tparam B A type parameter for the binary operator, a supertype of `A`.
    * @param op A binary operator that must be associative.
    * @return The result of applying reduce operator `op` between all the elements if the $coll is nonempty.
    * @throws UnsupportedOperationException
    * if this $coll is empty.
    */
  def reduce[B >: A](op: (B, B) => B): B = {
    val forward: VectorIterator[A] = this.iterator
    var acc: B = forward.next()
    while (forward.hasNext) acc = op(acc, forward.next())
    acc
  }

  /** Reduces the elements of this $coll, if any, using the specified
    * associative binary operator.
    *
    * $undefinedorder
    *
    * @tparam B A type parameter for the binary operator, a supertype of `A`.
    * @param op A binary operator that must be associative.
    * @return An option value containing result of applying reduce operator `op` between all
    *         the elements if the collection is nonempty, and `None` otherwise.
    */
  def reduceOption[B >: A](op: (A, B) => B): Option[B] = {
    if (this isEmpty) None
    else {
      val forward = this.iterator
      var acc: B = forward.next()
      while (forward.hasNext) acc = op(forward.next(), acc)
      Some(acc)
    }
  }

  /** Folds the elements of this $coll using the specified associative
    * binary operator.
    *
    * $undefinedorder
    * $willNotTerminateInf
    *
    * @tparam B a type parameter for the binary operator, a supertype of `A`.
    * @param z  a neutral element for the fold operation; may be added to the result
    *           an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
    *           0 for addition, or 1 for multiplication).
    * @param op a binary operator that must be associative.
    * @return the result of applying the fold operator `op` between all the elements and `z`, or `z` if this $coll is empty.
    */
  final def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val forward: VectorIterator[A] = this.iterator
    var acc: B = z
    while (forward.hasNext) acc = op(acc, forward.next())
    acc
  }

  /** Applies a binary operator to a start value and all elements of this $coll,
    * going left to right.
    *
    * Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the same as
    * `xs foldLeft z`.
    *
    * Examples:
    *
    * Note that the folding function used to compute b is equivalent to that used to compute c.
    * {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = (5 /: a)(_+_)
    *      b: Int = 15
    *
    *      scala> val c = (5 /: a)((x,y) => x + y)
    *      c: Int = 15
    * }}}
    *
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * @param   z  the start value.
    * @param   op the binary operator.
    * @tparam  B the result type of the binary operator.
    * @return the result of inserting `op` between consecutive elements of this $coll,
    *         going left to right with the start value `z` on the left:
    *         {{{
    *                                        op(...op(op(z, x_1), x_2), ..., x_n)
    *         }}}
    *         where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    */
  @tailrec final def /:[B](z: B)(op: (B, A) => B): B = {
    this.tail./:(op(z, this.head))(op)
  }

  /** Applies a binary operator to all elements of this $coll and a start value,
    * going right to left.
    *
    * Note: `:\` is alternate syntax for `foldRight`; `xs :\ z` is the same as
    * `xs foldRight z`.
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * Examples:
    *
    * Note that the folding function used to compute b is equivalent to that used to compute c.
    * {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = (a :\ 5)(_+_)
    *      b: Int = 15
    *
    *      scala> val c = (a :\ 5)((x,y) => x + y)
    *      c: Int = 15
    *
    * }}}
    *
    * @param   z  the start value
    * @param   op the binary operator
    * @tparam  B the result type of the binary operator.
    * @return the result of inserting `op` between consecutive elements of this $coll,
    *         going right to left with the start value `z` on the right:
    *         {{{
    *                                        op(x_1, op(x_2, ... op(x_n, z)...))
    *         }}}
    *         where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    */
  @tailrec final def :\[B](z: B)(op: (A, B) => B): B = {
    this.init.:\(op(this.last, z))(op)
  }


  /** Applies a binary operator to a start value and all elements of this $coll,
    * going left to right.
    *
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * @param   z  the start value.
    * @param   op the binary operator.
    * @tparam  B the result type of the binary operator.
    * @return the result of inserting `op` between consecutive elements of this $coll,
    *         going left to right with the start value `z` on the left:
    *         {{{
    *                                        op(...op(z, x_1), x_2, ..., x_n)
    *         }}}
    *         where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *         Returns `z` if this $coll is empty.
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val forward: VectorIterator[A] = this.iterator
    var acc: B = z
    while (forward.hasNext) acc = op(acc, forward.next())
    acc
  }

  /** Applies a binary operator to all elements of this $coll and a start value,
    * going right to left.
    *
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * @param   z  the start value.
    * @param   op the binary operator.
    * @tparam  B the result type of the binary operator.
    * @return the result of inserting `op` between consecutive elements of this $coll,
    *         going right to left with the start value `z` on the right:
    *         {{{
    *                                        op(x_1, op(x_2, ... op(x_n, z)...))
    *         }}}
    *         where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *         Returns `z` if this $coll is empty.
    */
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    val backward: VectorReverseIterator[A] = this.reverseiterator
    var acc: B = z
    while (backward.hasNext) acc = op(backward.next(), acc)
    acc
  }

  /** Aggregates the results of applying an operator to subsequent elements.
    *
    * This is a more general form of `fold` and `reduce`. It is similar to
    * `foldLeft` in that it doesn't require the result to be a supertype of the
    * element type. In addition, it allows parallel collections to be processed
    * in chunks, and then combines the intermediate results.
    *
    * `aggregate` splits the $coll into partitions and processes each
    * partition by sequentially applying `seqop`, starting with `z` (like
    * `foldLeft`). Those intermediate results are then combined by using
    * `combop` (like `fold`). The implementation of this operation may operate
    * on an arbitrary number of collection partitions (even 1), so `combop` may
    * be invoked an arbitrary number of times (even 0).
    *
    * As an example, consider summing up the integer values of a list of chars.
    * The initial value for the sum is 0. First, `seqop` transforms each input
    * character to an Int and adds it to the sum (of the partition). Then,
    * `combop` just needs to sum up the intermediate results of the partitions:
    * {{{
    *    List('a', 'b', 'c').aggregate(0)({ (sum, ch) => sum + ch.toInt }, { (p1, p2) => p1 + p2 })
    * }}}
    *
    * @tparam B the type of accumulated results
    * @param z      the initial value for the accumulated result of the partition - this
    *               will typically be the neutral element for the `seqop` operator (e.g.
    *               `Nil` for list concatenation or `0` for summation) and may be evaluated
    *               more than once
    * @param seqop  an operator used to accumulate results within a partition
    * @param combop an associative operator used to combine results from different partitions
    */
  def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B = { // TODO Proof of correctness
    var acc: B = z
    this.split.foreach(x =>
      acc = combop({
      var acc: B = z
      while(x.hasNext) acc = seqop(acc, x.next())
      acc
      } , acc))
    acc
  }

  def split: Seq[VectorIterator[A]] = {
    var splitted: ArrayBuffer[VectorIterator[A]] = new ArrayBuffer[VectorIterator[A]]
    val nsplits: Int = this.length / (1 << 5)
    var currentPos: Int = 0
    if (nsplits > 0) {
      var i: Int = 0
      while (i < nsplits) {
        val forward: VectorIterator[A] = new VectorIterator[A](currentPos, currentPos + 1 << 5)
        forward.initIteratorFrom(this)
        splitted += forward
        currentPos += 1 << 5
        i += 1
      }
      val forward: VectorIterator[A] = new VectorIterator[A](currentPos, this.length)
      forward.initIteratorFrom(this)
      splitted += forward
      splitted
    } else {
      Seq(this.iterator)
    }
  }

  /** Applies a binary operator to all elements of this $coll, going right to left.
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * @param  op the binary operator.
    * @tparam  B the result type of the binary operator.
    * @return the result of inserting `op` between consecutive elements of this $coll,
    *         going right to left:
    *         {{{
    *                                        op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
    *         }}}
    *         where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    * @throws UnsupportedOperationException if this $coll is empty.
    */
  def reduceRight[B >: A](op: (A, B) => B): B = {
    val reverse: VectorReverseIterator[A] = this.reverseiterator
    var acc: B = reverse.next()
    while (reverse.hasNext) acc = op(reverse.next(), acc)
    acc
  }

  /** Optionally applies a binary operator to all elements of this $coll, going
    * right to left.
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * @param  op the binary operator.
    * @tparam  B the result type of the binary operator.
    * @return an option value containing the result of `reduceRight(op)` if this $coll is nonempty,
    *         `None` otherwise.
    */
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = {
    if(this.isEmpty) None
    else {
      val reverse: VectorReverseIterator[A] = this.reverseiterator
      var acc: B = reverse.next()
      while (reverse.hasNext) acc = op(reverse.next(), acc)
      Some(acc)
    }
  }

  /** Applies a binary operator to all elements of this $coll, going right to left.
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * @param  op the binary operator.
    * @tparam  B the result type of the binary operator.
    * @return the result of inserting `op` between consecutive elements of this $coll,
    *         going right to left:
    *         {{{
    *                                        op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
    *         }}}
    *         where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    * @throws UnsupportedOperationException if this $coll is empty.
    */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val forward: VectorIterator[A] = this.iterator
    var acc: B = forward.next()
    while (forward.hasNext) acc = op(acc, forward.next())
    acc
  }

  /** Optionally applies a binary operator to all elements of this $coll, going left to right.
    * $willNotTerminateInf
    * $orderDependentFold
    *
    * @param  op the binary operator.
    * @tparam  B the result type of the binary operator.
    * @return an option value containing the result of `reduceLeft(op)` if this $coll is nonempty,
    *         `None` otherwise.
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    if(this.isEmpty) None
    else {
      val forward: VectorIterator[A] = this.iterator
      var acc: B = forward.next()
      while (forward.hasNext) acc = op(acc, forward.next())
      Some(acc)
    }
  }

  /** Counts the number of elements in the $coll which satisfy a predicate.
    *
    * @param p the predicate  used to test elements.
    * @return the number of elements satisfying the predicate `p`.
    */
  def count(p: A => Boolean): Int = {
    val forward: VectorIterator[A] = this.iterator
    var acc: Int = 0
    while (forward.hasNext) if (p(forward.next())) acc += 1
    acc
  }

  /** Tests whether a predicate holds for all elements of this $coll.
    *
    * $mayNotTerminateInf
    *
    * @param   p the predicate used to test elements.
    * @return `true` if this $coll is empty or the given predicate `p`
    *         holds for all elements of this $coll, otherwise `false`.
    */
  def forall(p: A => Boolean): Boolean = {
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) if (!p(forward.next())) return false
    true
  }

  /** Tests whether a predicate holds for at least one element of this $coll.
    *
    * $mayNotTerminateInf
    *
    * @param   p the predicate used to test elements.
    * @return `true` if the given predicate `p` is satisfied by at least one element of this $coll, otherwise `false`
    */
  def exists(p: A => Boolean): Boolean = {
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) if (p(forward.next())) return true
    false
  }

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    * $mayNotTerminateInf
    * $orderDependent
    *
    * @param p the predicate used to test elements.
    * @return an option value containing the first element in the $coll
    *         that satisfies `p`, or `None` if none exists.
    */
  def find(p: A => Boolean): Option[A] = {
    val forward: VectorIterator[A] = this.iterator
    while (forward.hasNext) {
      val x: A = forward.next()
      if (p(x)) return Some(x)
    }
    None
  }

  /** Copies the elements of this $coll to an array.
    * Fills the given array `xs` with values of this $coll.
    * Copying will stop once either the end of the current $coll is reached,
    * or the end of the target array is reached.
    *
    * @param  xs the array to fill.
    * @tparam B the type of the elements of the target array.
    * @usecase def copyToArray(xs: Array[A]): Unit
    * @inheritdoc
    *
    * $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B]): Unit = {
    val forward: VectorIterator[A] = this.iterator
    var index: Int = 0
    while(forward.hasNext) {
      xs(index) = forward.next()
      index += 1
    }
  }

  /** Copies the elements of this $coll to an array.
    * Fills the given array `xs` with values of this $coll, beginning at index `start`.
    * Copying will stop once either the end of the current $coll is reached,
    * or the end of the target array is reached.
    *
    * @param  xs    the array to fill.
    * @param  start the starting index.
    * @tparam B the type of the elements of the target array.
    * @usecase def copyToArray(xs: Array[A], start: Int): Unit
    * @inheritdoc
    *
    * $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit = {
    val forward: VectorIterator[A] = this.iterator(start, this.endIndex)
    var index: Int = 0
    while(forward.hasNext) {
      xs(index) = forward.next()
      index += 1
    }
  }

  /** Copies the elements of this $coll to an array.
    * Fills the given array `xs` with at most `len` elements of
    * this $coll, starting at position `start`.
    * Copying will stop once either the end of the current $coll is reached,
    * or the end of the target array is reached, or `len` elements have been copied.
    *
    * @param  xs    the array to fill.
    * @param  start the starting index.
    * @param  len   the maximal number of elements to copy.
    * @tparam B the type of the elements of the target array.
    * @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
    * @inheritdoc
    *
    * $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
    val forward: VectorIterator[A] = this.iterator(start, spire.math.min(start + len, this.endIndex))
    var index: Int = 0
    while(forward.hasNext) {
      xs(index) = forward.next()
      index += 1
    }
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
  def mkString(start: String, sep: String, end: String): String = addString(new StringBuilder(), start, sep, end).toString

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

  /** Converts this $coll to an array.
    *
    * @tparam B the type of the elements of the array. An `ClassTag` for
    *            this type must be available.
    * @return an array containing all elements of this $coll.
    * @usecase def toArray: Array[A]
    * @inheritdoc
    *
    * $willNotTerminateInf
    * @return an array containing all elements of this $coll.
    *         An `ClassTag` must be available for the element type of this $coll.
    */
  def toArray[B >: A : ClassTag]: Array[B] = {
    if (this.isTraversableAgain) {
      val result: Array[B] = new Array[B](size)
      this.copyToArray(result, 0)
      result
    } else this.toBuffer.toArray
  }

  /** Converts this $coll to a list.
    * $willNotTerminateInf
    *
    * @return a list containing all elements of this $coll.
    */
  def toList: List[A] = to[List]

  /** Converts this $coll to an indexed sequence.
    * $willNotTerminateInf
    *
    * @return an indexed sequence containing all elements of this $coll.
    */
  def toIndexedSeq: immutable.IndexedSeq[A] = to[immutable.IndexedSeq]

  /** Converts this $coll to a stream.
    *
    * @return a stream containing all elements of this $coll.
    */
  def toStream: Stream[A] = {
    val forward = this.iterator
    if (!forward.hasNext) Stream.empty[A]
    else Stream.cons(forward.next(), this.tail.toStream)
  }

  /** Returns an Iterator over the elements in this $coll.  Will return
    * the same Iterator if this instance is already an Iterator.
    * $willNotTerminateInf
    *
    * @return an Iterator containing all elements of this $coll.
    */
  def toIterator: VectorIterator[A] = this.iterator

  /** Uses the contents of this $coll to create a new mutable buffer.
    * $willNotTerminateInf
    *
    * @return a buffer containing all elements of this $coll.
    */
  def toBuffer[B >: A]: mutable.Buffer[B] = to[ArrayBuffer].asInstanceOf[mutable.Buffer[B]]

  /** Converts this $coll to an unspecified Traversable.  Will return
    * the same collection if this instance is already Traversable.
    * $willNotTerminateInf
    *
    * @return a Traversable containing all elements of this $coll.
    */
  def toTraversable: GenTraversable[A] = toStream

  /** Converts this $coll to an iterable collection.  Note that
    * the choice of target `Iterable` is lazy in this default implementation
    * as this `TraversableOnce` may be lazy and unevaluated (i.e. it may
    * be an iterator which is only traversable once).
    *
    * $willNotTerminateInf
    *
    * @return an `Iterable` containing all elements of this $coll.
    */
  def toIterable: GenIterable[A] = toStream

  /** Converts this $coll to a sequence. As with `toIterable`, it's lazy
    * in this default implementation, as this `TraversableOnce` may be
    * lazy and unevaluated.
    *
    * $willNotTerminateInf
    *
    * @return a sequence containing all elements of this $coll.
    */
  def toSeq: GenSeq[A] = toStream

  /** Converts this $coll to a set.
    * $willNotTerminateInf
    *
    * @return a set containing all elements of this $coll.
    */
  def toSet[B >: A]: Set[B] = to[immutable.Set].asInstanceOf[immutable.Set[B]]

  /** Converts this $coll to a map.  This method is unavailable unless
    * the elements are members of Tuple2, each ((T, U)) becoming a key-value
    * pair in the map.  Duplicate keys will be overwritten by later keys:
    * if this is an unordered collection, which key is in the resulting map
    * is undefined.
    *
    * @return a map containing all elements of this $coll.
    * @usecase def toMap[T, U]: Map[T, U]
    * @inheritdoc
    * $willNotTerminateInf
    * @return a map of type `immutable.Map[T, U]`
    *         containing all key/value pairs of type `(T, U)` of this $coll.
    */
  def toMap[K, V](implicit ev: A <:< (K, V)): GenMap[K, V] = {
    val build = immutable.Map.newBuilder[K, V]
    this.foreach(x => build += x)
    build.result()
  }


  /** Converts this $coll to a Vector.
    * $willNotTerminateInf
    *
    * @return a vector containing all elements of this $coll.
    */
  def toVector: ScalaVector[A] = to[ScalaVector]

  /** Converts this $coll into another by copying all elements.
    *
    * @tparam Col The collection type to build.
    * @return a new collection containing all elements of this $coll.
    * @usecase def to[Col[_] ]: Col[A]
    * @inheritdoc
    * $willNotTerminateInf
    * @return a new collection containing all elements of this $coll.
    */
  def to[Col[_]](implicit cbf: CanBuildFrom[Col[_], A, Col[A @uncheckedVariance]]): Col[A @uncheckedVariance] = {
    val build: mutable.Builder[A, Col[A]] = cbf()
    build ++= this.asInstanceOf[TraversableOnce[A]]
    build.result()
  }

  /** Helper Functions
    *
    */
  private[Immutable] def makeTransientIfNeeded(): Unit = {
    val _depth = depth
    if (_depth > 1 && transient.`unary_!`) {
      copyDisplaysAndNullFocusedBranch(_depth, focus.|(focusRelax))
      transient = true
    }
  }

  private[Immutable] def normalizeAndFocusOn(index: Int): Unit = {
    if (transient) {
      normalize(depth)
      transient = false
    }
    focusOn(index)
  }

  private def createSingletonVector[B >: A : ClassTag](elem: B): Vector[B] = {
    val resultVector: Vector[B] = new Vector[B](1)
    resultVector.initSingleton(elem)
    resultVector
  }

  private[Immutable] def prepend[B >: A : ClassTag](elem: B): Unit = {

    if (focusStart != 0 || (focus & -32) != 0) {
      normalizeAndFocusOn(0)
    }

    val d0: Leaf = display0
    if (d0.length < 32) {
      prependOnCurrentBlock(elem, d0.asInstanceOf[Array[B]])
    }
    else {
      prependFrontNewBlock(elem)
    }
  }

  private def prependOnCurrentBlock[B >: A](elem: B,
                                            old_d0: Array[B]): Unit = {
    val new_length: Int = old_d0.length + 1
    focusEnd = new_length

    val new_d0: Leaf = new Leaf(new_length)
    new_d0.update(0, elem.asInstanceOf[A])
    System.arraycopy(old_d0, 0, new_d0, 1, new_length - 1)
    display0 = new_d0

    makeTransientIfNeeded()
  }

  private def prependFrontNewBlock[B >: A](elem: B)(implicit ct: ClassTag[B]): Unit = {
    var currentDepth = focusDepth
    var display: Node = currentDepth match {
      case 1 => display1
      case 2 => display2
      case 3 => display3
      case 4 => display4
      case 5 => display5
      case 6 => display6
      case 7 => display7
    }

    while (display != null && display.length == 33) {
      currentDepth += 1
      currentDepth match {
        case 1 => display = display1
        case 2 => display = display2
        case 3 => display = display3
        case 4 => display = display4
        case 5 => display = display5
        case 6 => display = display6
        case 7 => display = display7
        case _ => throw new IllegalStateException()
      }
    }

    val oldDepth: Int = depth
    val _transient: Boolean = transient
    setupNewBlockInInitBranch(currentDepth, _transient)(ct.asInstanceOf[ClassTag[A]])
    if (oldDepth == depth) {
      var i: Int = currentDepth
      if (i < oldDepth) {
        val _focusDepth: Int = focusDepth
        var display: Node = i match {
          case 1 => display1
          case 2 => display2
          case 3 => display3
          case 4 => display4
          case 5 => display5
          case 6 => display6
          case 7 => display7
        }
        do {
          val displayLen: Int = display.length - 1
          val newSizes: Array[Int] = {
            if (i >= _focusDepth) {
              makeTransientSizes(display(displayLen).asInstanceOf[Array[Int]], 1)
            }
            else {
              null
            }
          }
          val newDisplay: Node = new Node(display.length)

          System.arraycopy(display, 0, newDisplay, 0, displayLen - 1)
          if (i >= _focusDepth) {
            newDisplay.update(displayLen, newSizes)
          }
          i match {
            case 2 =>
              display1 = newDisplay
              display = display2
            case 3 =>
              display2 = newDisplay
              display = display3
            case 4 =>
              display3 = newDisplay
              display = display4
            case 5 =>
              display4 = newDisplay
              display = display5
            case 6 =>
              display5 = newDisplay
              display = display6
            case 7 =>
              display6 = newDisplay
              display = display7
          }
          i += 1
        } while (i < oldDepth)
      }
    }
    initFocus(0, 0, 1, 1, 0)
    display0.update(0, elem.asInstanceOf[A])
    transient = true
  }

  private[Immutable] def append[B >: A : ClassTag](elem: B, _endIndex: Int): Unit = {
    if ((focusStart + focus ^ _endIndex - 1) >= 32)
      normalizeAndFocusOn(_endIndex - 1)

    val elemIndexInBlock = _endIndex.-(focusStart).&(31)
    if (elemIndexInBlock != 0)
      appendOnCurrentBlock(elem, elemIndexInBlock)
    else
      appendBackNewBlock(elem, elemIndexInBlock)
  }

  private def appendOnCurrentBlock[B >: A](elem: B,
                                           elemIndexInBlock: Int): Unit = {
    focusEnd = endIndex
    val d0: Leaf = new Leaf(elemIndexInBlock + 1)
    System.arraycopy(display0, 0, d0, 0, elemIndexInBlock)
    d0.update(elemIndexInBlock, elem.asInstanceOf[A])
    display0 = d0
    makeTransientIfNeeded()
  }

  private def appendBackNewBlock[B >: A](elem: B,
                                         elemIndexInBlock: Int)(implicit m: ClassTag[B]): Unit = {
    val oldDepth: Int = depth
    val newRelaxedIndex: Int = endIndex - 1 - focusStart + focusRelax
    val focusJoined: Int = focus | focusRelax
    val xor: Int = newRelaxedIndex ^ focusJoined
    val _transient: Boolean = transient
    setupNewBlockInNextBranch(xor, _transient)(m.asInstanceOf[ClassTag[A]])

    if (oldDepth == depth) {
      var i: Int = {
        if (xor < (1 << 10)) {
          2
        }
        else if (xor < (1 << 15)) {
          3
        }
        else if (xor < (1 << 20)) {
          4
        }
        else if (xor < (1 << 25)) {
          5
        }
        else if (xor < (1 << 30)) {
          6
        }
        else if (xor < (1 << 35)) {
          7
        }
        else {
          7
        }
      }

      if (i < oldDepth) {
        val _focusDepth: Int = focusDepth
        var display: Node = i match {
          case 1 => display1
          case 2 => display2
          case 3 => display3
          case 4 => display4
          case 5 => display5
          case 6 => display6
          case 7 => display7
        }
        do {
          val displayLen: Int = display.length - 1
          val newSizes: Array[Int] = {
            if (i >= _focusDepth) {
              makeTransientSizes(display(displayLen).asInstanceOf[Array[Int]], displayLen - 1)
            }
            else {
              null
            }
          }
          val newDisplay: Node = new Node(display.length)

          System.arraycopy(display, 0, newDisplay, 0, displayLen - 1)
          if (i >= _focusDepth) {
            newDisplay.update(displayLen, newSizes)
          }
          i match {
            case 2 =>
              display1 = newDisplay
              display = display2
            case 3 =>
              display2 = newDisplay
              display = display3
            case 4 =>
              display3 = newDisplay
              display = display4
            case 5 =>
              display4 = newDisplay
              display = display5
            case 6 =>
              display5 = newDisplay
              display = display6
            case 7 =>
              display6 = newDisplay
              display = display7
          }
          i += 1
        } while (i < oldDepth)
      }
    }
    if (oldDepth == focusDepth)
      initFocus(endIndex - 1, 0, endIndex, depth, 0)
    else
      initFocus(endIndex - 1, endIndex - 1, endIndex, 1, newRelaxedIndex & -32)
    display0.update(elemIndexInBlock, elem.asInstanceOf[A])
    transient = true
  }

  private def occurrences[B >: A]: mutable.Map[B, Int] = {
    val occurrence: mutable.HashMap[B, Int] = new mutable.HashMap[B, Int] {
      override def default(k: B) = 0
    }
    val forward = this.iterator
    while (forward.hasNext) occurrence(forward.next()) += 1
    occurrence
  }

  private[Immutable] def concatenate[B >: A](currentSize: Int,
                                             that: Vector[B]): Unit = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }

    if (that.transient) {
      that.normalize(that.depth)
      that.transient = false
    }

    this.focusOn(currentSize - 1)
    spire.math.max(this.depth, that.depth) match {

      case 1 =>

        val concat: Node = rebalancedLeafs(display0, that.display0.asInstanceOf[Leaf])
        initFromRoot(concat, 1)

      case 2 =>

        var d0: Leaf = null
        var d1: Node = null

        if (((that.focus | that.focusRelax) & -32) == 0) {

          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]

        } else {

          if (that.display1 != null)
            d1 = that.display1
          if (d1 == null)
            d0 = that.display0.asInstanceOf[Leaf]
          else
            d0 = d1(0).asInstanceOf[Leaf]

          var concat: Node = rebalancedLeafs(this.display0, d0)
          concat = rebalanced(this.display1, concat, that.display1, 2)

          if (concat.length == 2)
            initFromRoot(concat(0).asInstanceOf[Node], 2)
          else
            initFromRoot(withComputedSizes(concat, 3), 3)
        }

      case 3 =>

        var d0: Leaf = null
        var d1: Node = null
        var d2: Node = null

        if ((that.focus & -32) == 0) {
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]
        } else {
          if (that.display2 != null)
            d2 = that.display2

          if (d2 == null)
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1 == null)
            d0 = that.display0.asInstanceOf[Leaf]
          else
            d0 = d1(0).asInstanceOf[Leaf]
        }

        var concat: Node = rebalancedLeafs(this.display0, d0)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, that.display2, 3)

        if (concat.length == 2)
          initFromRoot(concat(0).asInstanceOf[Node], 3)
        else
          initFromRoot(withComputedSizes(concat, 4), 4)

      case 4 =>

        var d0: Leaf = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null

        if ((that.focus & -32) == 0) {

          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]

        } else {

          if (that.display3 != null)
            d3 = that.display3

          if (d3 == null)
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2 == null)
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1 == null)
            d0 = that.display0.asInstanceOf[Leaf]
          else
            d0 = d1(0).asInstanceOf[Leaf]

        }

        var concat: Node = rebalancedLeafs(this.display0, d0)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, that.display3, 4)

        if (concat.length == 2)
          initFromRoot(concat(0).asInstanceOf[Node], 4)
        else
          initFromRoot(withComputedSizes(concat, 5), 5)

      case 5 =>

        var d0: Leaf = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null
        var d4: Node = null

        if ((that.focus & -32) == 0) {
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]
        } else {
          if (that.display4 != null)
            d4 = that.display4
          if (d4 == null)
            d3 = that.display3
          else
            d3 = d4(0).asInstanceOf[Node]
          if (d3 == null)
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2 == null)
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1 == null)
            d0 = that.display0.asInstanceOf[Leaf]
          else
            d0 = d1(0).asInstanceOf[Leaf]
        }

        var concat: Node = rebalancedLeafs(this.display0, d0)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, d4, 5)

        if (concat.length == 2)
          initFromRoot(concat(0).asInstanceOf[Node], 5)
        else
          initFromRoot(withComputedSizes(concat, 6), 6)

      case 6 =>

        var d0: Leaf = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null
        var d4: Node = null
        var d5: Node = null

        if ((that.focus & -32) == 0) {
          d5 = that.display5
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]
        } else {

          if (that.display5 != null)
            d5 = that.display5
          if (d5 == null)
            d4 = that.display4
          else
            d4 = d5(0).asInstanceOf[Node]
          if (d4 == null)
            d3 = that.display3
          else
            d3 = d4(0).asInstanceOf[Node]
          if (d3 == null)
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2 == null)
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1 == null)
            d0 = that.display0.asInstanceOf[Leaf]
          else
            d0 = d1(0).asInstanceOf[Leaf]
        }

        var concat: Node = rebalancedLeafs(this.display0, d0)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, d4, 5)
        concat = rebalanced(this.display5, concat, that.display5, 6)

        if (concat.length == 2)
          initFromRoot(concat(0).asInstanceOf[Node], 6)
        else
          initFromRoot(withComputedSizes(concat, 7), 7)

      case 7 =>

        var d0: Leaf = null
        var d1: Node = null
        var d2: Node = null
        var d3: Node = null
        var d4: Node = null
        var d5: Node = null
        var d6: Node = null

        if ((that.focus & -32) == 0) {

          d6 = that.display6
          d5 = that.display5
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]

        } else {

          if (that.display6 != null)
            d6 = that.display6

          if (d6 == null)
            d5 = that.display5
          else
            d5 = d6(0).asInstanceOf[Node]
          if (d5 == null)
            d4 = that.display4
          else
            d4 = d5(0).asInstanceOf[Node]
          if (d4 == null)
            d3 = that.display3
          else
            d3 = d4(0).asInstanceOf[Node]
          if (d3 == null)
            d2 = that.display2
          else
            d2 = d3(0).asInstanceOf[Node]
          if (d2 == null)
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1 == null)
            d0 = that.display0.asInstanceOf[Leaf]
          else
            d0 = d1(0).asInstanceOf[Leaf]
        }

        var concat: Node = rebalancedLeafs(this.display0, d0)
        concat = rebalanced(this.display1, concat, d1, 2)
        concat = rebalanced(this.display2, concat, d2, 3)
        concat = rebalanced(this.display3, concat, d3, 4)
        concat = rebalanced(this.display4, concat, d4, 5)
        concat = rebalanced(this.display5, concat, d5, 6)
        concat = rebalanced(this.display6, concat, that.display6, 7)

        if (concat.length == 2)
          initFromRoot(concat(0).asInstanceOf[Node], 7)
        else
          initFromRoot(withComputedSizes(concat, 8), 8)

      case _ => throw new IllegalStateException("depth = " + spire.math.max(this.depth, that.depth).toString)
    }
  }

  private def rebalanced[B >: A](displayLeft: Node,
                                 concat: Node,
                                 displayRight: Node,
                                 currentDepth: Int)(implicit ct: ClassTag[B]): Node = {
    val leftLength: Int = {
      if (displayLeft == null)
        0
      else
        displayLeft.length - 1
    }

    val concatLength: Int = {
      if (concat == null)
        0
      else
        concat.length - 1
    }

    val rightLength: Int = {
      if (displayRight == null)
        0
      else
        displayRight.length - 1
    }

    val branching: Int = computeBranching(displayLeft, concat, displayRight, currentDepth)

    val top: Node = new Node(branching >> 10 + (if ((branching & 1 << 10 - 1) == 0) 1 else 2))
    var mid: Node = new Node(if ((branching >> 10) == 0) (branching + 31) >> 5 + 1 else 33)

    var bot: Node = null
    var iSizes: Int = 0
    var iTop: Int = 0
    var iMid: Int = 0
    var iBot: Int = 0
    var i: Int = 0
    var j: Int = 0
    var d: Int = 0

    var currentDisplay: Node = null
    var displayEnd: Int = 0

    do {
      d match {
        case 0 =>
          if (displayLeft != null) {
            currentDisplay = displayLeft
            if (concat == null)
              displayEnd = leftLength
            else
              displayEnd = leftLength - 1
          }
        case 1 =>
          if (concat == null)
            displayEnd = 0
          else {
            currentDisplay = concat
            displayEnd = concatLength
          }
          i = 0
        case 2 =>
          if (displayRight != null) {
            currentDisplay = displayRight
            displayEnd = rightLength
            i = if (concat == null) 0 else 1
          }
      }

      while (i < displayEnd) {
        val displayValue: Node = currentDisplay(i).asInstanceOf[Node]
        val displayValueEnd: Int = {
          if (currentDepth == 2)
            displayValue.length
          else
            displayValue.length - 1
        }
        if (((iBot | j) == 0) && (displayValueEnd == 32)) {
          if ((currentDepth != 2) && (bot != null)) {
            withComputedSizes(bot, currentDepth - 1)
            bot = null
          }
          mid.update(iMid, displayValue)
          i += 1
          iMid += 1
          iSizes += 1
        } else {
          val numElementsToCopy: Int = spire.math.min(displayValueEnd - j, 32 - iBot)

          if (iBot == 0) {
            if ((currentDepth != 2) && (bot != null))
              withComputedSizes(bot, currentDepth - 1)

            bot = new Node(
              spire.math.min(branching - (iTop << 10) - (iMid << 5), 32)
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
              withComputedSizes(bot, currentDepth - 1)
          }
        }
        if (iMid == 32) {
          top.update(iTop, withComputedSizes(mid, currentDepth))
          iTop += 1
          iMid = 0
          val remainingBranches =
            branching - (iTop << 5 | (iMid << 5) | iBot)
          if (remainingBranches > 0)
            mid = new Node(
              if ((remainingBranches >> 10) == 0)
                remainingBranches + 63 >> 5 else 33
            )
          else
            mid = null
        }
      }
      d += 1
    } while (d < 3)

    if (currentDepth != 2 && bot != null)
      withComputedSizes(bot, currentDepth - 1)

    if (mid != null)
      top.update(iTop, withComputedSizes(mid, currentDepth))
    top
  }

  private def rebalancedLeafs[B >: A](displayLeft: Array[B],
                                      displayRight: Array[B]
                                     ): Node = {
    val leftLength = displayLeft.length
    val rightLength = displayRight.length

    if (leftLength == 32) {
      val top = new Node(3)
      top.update(0, displayLeft)
      top.update(1, displayRight)
      top
    } else {
      if (leftLength + rightLength <= 32) {
        val mergedDisplay = new Node(leftLength + rightLength)
        System.arraycopy(displayLeft, 0, mergedDisplay, 0, leftLength)
        System.arraycopy(displayRight, 0, mergedDisplay, leftLength, rightLength)

        val top = new Node(2)
        top.update(0, mergedDisplay)
        top
      } else {
        val top = new Node(3)
        val arr0 = new Leaf(32)
        val arr1 = new Leaf(leftLength + rightLength - 32)

        System.arraycopy(displayLeft, 0, arr0, 0, leftLength)
        System.arraycopy(displayRight, 0, arr0, leftLength, 32 - leftLength)
        System.arraycopy(displayRight, 32 - leftLength, arr1, 0, rightLength - 32 + leftLength)

        top.update(0, arr0)
        top.update(1, arr1)
        top
      }
    }
  }

  private def computeBranching(displayLeft: Node,
                               concat: Node,
                               displayRight: Node,
                               currentDepth: Int): Int = {
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
      branching = leftLength + concatLength + rightLength

      if (leftLength != 0)
        branching -= 1

      if (rightLength != 0)
        branching -= 1

    } else {
      var i = 0

      while (i < leftLength - 1) {
        branching += displayLeft(i).asInstanceOf[Node].length
        i += 1
      }

      i = 0

      while (i < concatLength) {
        branching += concat(i).asInstanceOf[Leaf].length
        i += 1
      }

      i = 1

      while (i < rightLength) {
        branching += displayRight(i).asInstanceOf[Node].length
        i += 1
      }

      if (currentDepth != 2) {
        branching -= leftLength + concatLength + rightLength

        if (leftLength != 0)
          branching += 1

        if (rightLength != 0)
          branching += 1
      }
    }
    branching
  }

  private def takeFront0(n: Int): Vector[A] = {

    if (transient) {
      normalize(depth)
      transient = false
    }

    val vector = new Vector[A](n)

    vector.initWithFocusFrom(this)

    if (depth > 1) {
      vector.focusOn(n - 1)
      val d0len = (vector.focus & 31) + 1
      if (d0len != 32) {
        val d0 = new Leaf(d0len)
        System.arraycopy(vector.display0, 0, d0, 0, d0len)
        vector.display0 = d0
      }

      val cutIndex = vector.focus | vector.focusRelax
      vector.cleanTopTake(cutIndex)
      vector.focusDepth = spire.math.min(vector.depth, vector.focusDepth)

      if (vector.depth > 1) {
        vector.copyDisplays(vector.focusDepth, cutIndex)
        var i = vector.depth
        var offset = 0
        var display: Node = null

        while (i > vector.focusDepth) {
          i match {
            case 2 => display = vector.display1
            case 3 => display = vector.display2
            case 4 => display = vector.display3
            case 5 => display = vector.display4
            case 6 => display = vector.display5
            case 7 => display = vector.display6
          }
          val oldSizes = display(display.length - 1).asInstanceOf[Array[Int]]
          val newLen = (vector.focusRelax >> 5 * (i - 1) & 31) + 1
          val newSizes = new Array[Int](newLen)
          System.arraycopy(oldSizes, 0, newSizes, 0, newLen - 1)
          newSizes.update(newLen - 1, n - offset)
          if (newLen > 1)
            offset += newSizes(newLen - 2)

          val newDisplay = new Node(newLen + 1)
          System.arraycopy(display, 0, newDisplay, 0, newLen)
          newDisplay.update(newLen - 1, null)
          newDisplay.update(newLen, newSizes)
          i match {
            case 1 => vector.display1 = newDisplay
            case 2 => vector.display2 = newDisplay
            case 3 => vector.display3 = newDisplay
            case 4 => vector.display4 = newDisplay
            case 5 => vector.display5 = newDisplay
            case 6 => vector.display6 = newDisplay
            case 7 => vector.display7 = newDisplay
          }
          i -= 1
        }
        vector.stabilizeDisplayPath(vector.depth, cutIndex)
        vector.focusEnd = n
      } else
        vector.focusEnd = n
    } else if (n != 32) {
      val d0 = new Leaf(n)
      System.arraycopy(vector.display0, 0, d0, 0, n)
      vector.display0 = d0
      vector.initFocus(0, 0, n, 1, 0)
    }
    vector
  }

  private def dropFront0(n: Int): Vector[A] = { // Need to fix, since depth = 1 means there is still a node

    if (transient) {
      normalize(depth)
      transient = false
    }
    val vector: Vector[A] = new Vector[A](this.endIndex - n)
    vector.initWithFocusFrom(this)

    if (vector.depth >= 1) { // vector.depth > 1
      vector.focusOn(n)
      val cutIndex: Int = vector.focus | vector.focusRelax
      val d0Start: Int = cutIndex & 31
      if (d0Start != 0) {
        val d0length: Int = vector.display0.length - d0Start
        val d0: Leaf = new Leaf(d0length)
        System.arraycopy(vector.display0, d0Start, d0, 0, d0length)
        vector.display0 = d0
      }

      vector.cleanTopDrop(cutIndex)

      if (vector.depth >= 1) { // vector.depth > 1
        var i: Int = 2
        var display: Node = vector.display1
        while (i <= vector.depth) {
          val splitStart = cutIndex >> 5 * (i - 1) & 31
          val newLen = display.length - splitStart - 1
          val newDisplay = new Node(newLen + 1)
          System.arraycopy(display, splitStart + 1, newDisplay, 1, newLen - 1)
          i match {
            case 2 =>
              newDisplay.update(0, vector.display0)
              vector.display1 = withComputedSizes(newDisplay, 2)
              display = vector.display2
            case 3 =>
              newDisplay.update(0, vector.display1)
              vector.display2 = withComputedSizes(newDisplay, 3)
              display = vector.display3
            case 4 =>
              newDisplay.update(0, vector.display2)
              vector.display3 = withComputedSizes(newDisplay, 4)
              display = vector.display4
            case 5 =>
              newDisplay.update(0, vector.display3)
              vector.display4 = withComputedSizes(newDisplay, 5)
              display = vector.display5
            case 6 =>
              newDisplay.update(0, vector.display4)
              vector.display5 = withComputedSizes(newDisplay, 6)
              display = vector.display6
            case 7 =>
              newDisplay.update(0, vector.display5)
              vector.display6 = withComputedSizes(newDisplay, 7)
          }
          i += 1
        }
      }

      vector.initFocus(0, 0, vector.display0.length, 1, 0)
    } else {
      val newLen: Int = vector.display0.length - n
      val d0: Leaf = new Leaf(newLen)
      System.arraycopy(vector.display0, n, d0, 0, newLen)
      vector.display0 = d0
      vector.initFocus(0, 0, newLen, 1, 0)
    }
    vector
  }

  private[Immutable] final def assertVectorInvariant(): Unit = { // TODO Need to update to to my design of the data structure
    if (Vector.compileAssertions) {
      assert(0 <= depth && depth <= 6, depth)
      assert(isEmpty == (depth == 0), (isEmpty, depth))
      assert(isEmpty == (length == 0), (isEmpty, length))
      assert(length == endIndex, (length, endIndex))
      assert((depth <= 0 && display0 == null) || (depth > 0 && display0 != null))
      assert((depth <= 1 && display1 == null) || (depth > 0 && display1 != null))
      assert((depth <= 2 && display2 == null) || (depth > 0 && display2 != null))
      assert((depth <= 3 && display3 == null) || (depth > 0 && display3 != null))
      assert((depth <= 4 && display4 == null) || (depth > 0 && display4 != null))
      assert((depth <= 5 && display5 == null) || (depth > 0 && display5 != null))

      if (!transient) {
        if (display5 != null) {
          assert(display4 != null)
          if (focusDepth <= 5) assert(display5((focusRelax >> 25) & 31) == display4)
          else assert(display5((focus >> 25) & 31) == display4)
        }
        if (display4 != null) {
          assert(display3 != null)
          if (focusDepth <= 4) assert(display4((focusRelax >> 20) & 31) == display3)
          else assert(display4((focus >> 20) & 31) == display3)
        }
        if (display3 != null) {
          assert(display2 != null)
          if (focusDepth <= 3) assert(display3((focusRelax >> 15) & 31) == display2)
          else assert(display3((focus >> 15) & 31) == display2)
        }
        if (display2 != null) {
          assert(display1 != null)
          if (focusDepth <= 2) assert(display2((focusRelax >> 10) & 31) == display1)
          else assert(display2((focus >> 10) & 31) == display1)
        }
        if (display1 != null) {
          assert(display0 != null)
          if (focusDepth <= 1) assert(display1((focusRelax >> 5) & 31) == display0)
          else assert(display1((focus >> 5) & 31) == display0)
        }
      } else {
        assert(depth > 1)
        if (display5 != null) {
          assert(display4 != null)
          if (focusDepth <= 5) assert(display5((focusRelax >> 25) & 31) == null)
          else assert(display5((focus >> 25) & 31) == null)
        }
        if (display4 != null) {
          assert(display3 != null)
          if (focusDepth <= 4) assert(display4((focusRelax >> 20) & 31) == null)
          else assert(display4((focus >> 20) & 31) == null)
        }
        if (display3 != null) {
          assert(display2 != null)
          if (focusDepth <= 3) assert(display3((focusRelax >> 15) & 31) == null)
          else assert(display3((focus >> 15) & 31) == null)
        }
        if (display2 != null) {
          assert(display1 != null)
          if (focusDepth <= 2) assert(display2((focusRelax >> 10) & 31) == null)
          else assert(display2((focus >> 10) & 31) == null)
        }
        if (display1 != null) {
          assert(display0 != null)
          if (focusDepth <= 1) assert(display1((focusRelax >> 5) & 31) == null)
          else assert(display1((focus >> 5) & 31) == null)
        }
      }


      assert(0 <= focusStart && focusStart <= focusEnd && focusEnd <= endIndex, (focusStart, focusEnd, endIndex))
      assert(focusStart == focusEnd || focusEnd != 0, (focusStart, focusEnd))
      assert(0 <= focusDepth && focusDepth <= depth, scala.Tuple2(focusDepth, depth))

      def checkSizes(node: Node, currentDepth: Int, _endIndex: Int): Unit = {
        if (currentDepth > 1) {
          if (node != null) {
            val sizes = node(node.length - 1).asInstanceOf[Size]
            if (sizes != null) {
              assert(node.length == sizes.length + 1)
              if (!transient)
                assert(sizes(sizes.length - 1) == _endIndex, (sizes(sizes.length - 1), _endIndex))

              var i = 0
              while (i < sizes.length - 1) {
                checkSizes(node(i).asInstanceOf[Node], currentDepth - 1, sizes(i) - (if (i == 0) 0 else sizes(i - 1)))
                i += 1
              }
              checkSizes(node(node.length - 2).asInstanceOf[Node], currentDepth - 1, if (sizes.length > 1) sizes(sizes.length - 1) - sizes(sizes.length - 2) else sizes(sizes.length - 1))
            } else {
              var i = 0
              while (i < node.length - 2) {
                checkSizes(node(i).asInstanceOf[Node], currentDepth - 1, 1 << (5 * (currentDepth - 1)))
                i += 1
              }
              val expectedLast = _endIndex - (1 << (5 * (currentDepth - 1))) * (node.length - 2)
              assert(1 <= expectedLast && expectedLast <= (1 << (5 * currentDepth)))
              checkSizes(node(node.length - 2).asInstanceOf[Node], currentDepth - 1, expectedLast)
            }
          } else {
            assert(transient)
          }
        } else if (node != null) {
          assert(node.length == _endIndex)
        } else {
          assert(transient)
        }
      }
      depth match {
        case 1 => checkSizes(display1, 1, endIndex)
        case 2 => checkSizes(display2, 2, endIndex)
        case 3 => checkSizes(display3, 3, endIndex)
        case 4 => checkSizes(display4, 4, endIndex)
        case 5 => checkSizes(display5, 5, endIndex)
        case 6 => checkSizes(display6, 6, endIndex)
        case 7 => checkSizes(display7, 7, endIndex)
        case _ => ()
      }
    }
  }

}
