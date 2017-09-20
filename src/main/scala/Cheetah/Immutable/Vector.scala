package Cheetah.Immutable

//import Cheetah.Immutable.Vector.{Coll, ReusableCBF}

import Cheetah.Immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.{CanBuildFrom, FilterMonadic, GenericCompanion, IndexedSeqFactory}
import scala.collection.parallel.{Combiner, ParIterable}
import scala.collection.{GenIterable, GenTraversableOnce, IndexedSeqLike, Iterable, IterableLike, IterableView, Iterator, Traversable, TraversableLike, breakOut, immutable, mutable}
import scala.reflect.ClassTag
import scala.{specialized => sp}
import scala.{Vector => Vec}
import spire.syntax.all._

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

object Vector extends scala.collection.generic.IndexedSeqFactory[Vector] {

  def newBuilder[@sp A]: mutable.Builder[A, Vector[A]] =
    new VectorBuilder[A]()

  implicit def canBuildFrom[@sp A: ClassTag]
    : scala.collection.generic.CanBuildFrom[Coll, A, Vector[A]] =
      ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  lazy private val EMPTY_VECTOR = new Vector[Nothing](0)

  override def empty[A]: Vector[A] = EMPTY_VECTOR

  final lazy private[Immutable] val emptyTransientBlock = new Array[AnyRef](2)
}


final class Vector[@sp +A](override private[Immutable] val endIndex: Int)(implicit ct: ClassTag[A])
  extends Traversable[A]
    with TraversableLike[A, Vector[A]]
    with Iterable[A]
    with IndexedSeqFactory[Vector]
    with IterableLike[A, Vector[A]]
    with VectorPointer[A @uncheckedVariance]
    with Serializable { self =>

  private[Immutable] var transient: Boolean = false

  def newBuilder[@sp B >: A]: VectorBuilder[B] = new VectorBuilder[B]()

  implicit def canBuildFrom[@sp B >: A]: scala.collection.generic.CanBuildFrom[Coll, B, Vector[B]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[B]]

  lazy private val EMPTY_VECTOR = new Vector[Nothing](0)

  override def empty[@sp B >: A]: Vector[B] = EMPTY_VECTOR

  final lazy private[Immutable] val emptyTransientBlock = new Node(2)

  /*Methods from TraversableLike */

  override protected[this] type Self = Vector[A]

  override def repr: Vector[A] = this.asInstanceOf[Self]

  override def size: Int = endIndex

  def sizeCompare(len: Int): Int = endIndex - len

  override def par: ParVector[A] = new ParVector[A](this)

  def apply(index: Int): A = {
    val _focusStart = this.focusStart
    if (_focusStart <= index && index < this.focusEnd) {
      val indexInFocus = index - _focusStart
      getElem(indexInFocus, indexInFocus ^ this.focus)
    } else if (0 <= index && index < endIndex)
      getElementFromRoot(index)
    else
      throw new IndexOutOfBoundsException(index.toString)
  }

  override def iterator: VectorIterator[A] = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }
    val it = new VectorIterator[A](0, endIndex)
    it.initIteratorFrom(this)
    it
  }

  def reverseIterator: VectorReverseIterator[A] = {
    if (this.transient) {
      this.normalize(this.depth)
      this.transient = false
    }
    val rit = new VectorReverseIterator[A](0, endIndex)
    rit.initIteratorFrom(this)
    rit
  }

  def ++[@sp B >: A : ClassTag, @sp That](that: GenTraversableOnce[B])
                                         (implicit bf: CanBuildFrom[Vector[A], B, That]): That = {

    if (bf.eq(IndexedSeq.ReusableCBF)) {
        if (that.isEmpty)
          this.asInstanceOf[That]
        else that match {
          case thatVec: Vector[B] =>
            if (this.endIndex == 0)
              thatVec.asInstanceOf[That]
            else {
              val newVec = new Vector[B](this.endIndex + thatVec.endIndex)
              newVec.initWithFocusFrom(this.asInstanceOf[Vector[B]])
              newVec.transient = this.transient
              newVec.concatenate(this.endIndex, thatVec)
              newVec.asInstanceOf[That]
            }
          case _ =>
            val b: VectorBuilder[B] = newBuilder[B] // TODO Make sure this is working at it is supposed to
            if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
            b ++= thisCollection
            b ++= that.seq
            b.result.asInstanceOf[That]
        }
      } else {
      val b: VectorBuilder[B] = newBuilder[B]
      if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
      b ++= thisCollection
      b ++= that.seq
      b.result.asInstanceOf[That]
    }
  }

  def ++:[@sp B >: A, @sp That](that: TraversableOnce[B])
                               (implicit bf: CanBuildFrom[Vector[A], B, That], ct: ClassTag[B]): That = {
    val b: VectorBuilder[B] = newBuilder[B]
    if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= that
    b ++= thisCollection
    b.result.asInstanceOf[That]
  }

  def ++:[@sp B >: A : ClassTag, @sp That](that: Traversable[B])
                                          (implicit bf: CanBuildFrom[Vector[A], B, That]): That = (that ++ this)(breakOut)

  // Append
  def :+[@sp B >: A, That](elem: B)
                          (implicit bf: CanBuildFrom[Vector[A], B, That], ct: ClassTag[B]): That = {
    if (bf.eq(IndexedSeq.ReusableCBF)) {
      val _endIndex = this.endIndex
      if (_endIndex != 0) {
        val resultVector: Vector[B] = new Vector[B](_endIndex + 1)
        resultVector.transient = this.transient
        resultVector.initWithFocusFrom(this.asInstanceOf[Vector[B]])
        resultVector.append(elem, _endIndex)
        resultVector.asInstanceOf[That]
      } else
        createSingletonVector(elem).asInstanceOf[That]
    } else {
      val b: VectorBuilder[B] = newBuilder[B]
      b ++= thisCollection
      b += elem
      b.result.asInstanceOf[That]
    }
  }

  // Prepend
  def +:[@sp B >: A, That](elem: B)
                          (implicit bf: CanBuildFrom[Vector[A], B, That], ct: ClassTag[B]): That = {
    if (bf.eq(IndexedSeq.ReusableCBF)) {
      val _endIndex = this.endIndex
      if (_endIndex != 0) {
        val resultVector = new Vector[B](_endIndex + 1)
        resultVector.transient = this.transient
        resultVector.initWithFocusFrom(this.asInstanceOf[Vector[B]])
        resultVector.prepend(elem)
        resultVector.asInstanceOf[That]
      } else
        createSingletonVector(elem).asInstanceOf[That]
    } else {
      val b: VectorBuilder[B] = newBuilder[B]
      b += elem
      b ++= thisCollection
      b.result.asInstanceOf[That]
    }
  }

  /*
  * Map Like Operations
  * */

  override def foreach[@sp B](f: (A) => B): Unit =
    cfor(0)(_ < endIndex, _ + 1) { i:Int => f(this(i)) } // TODO Implement in the object and optimize using C++/CUDA via JavaCPP

  def map[@sp B >: A, @sp That](f: (A) => B)
                               (implicit bf: CanBuildFrom[Vector[A], B, That], ct: ClassTag[B]): That = {
    val b: VectorBuilder[B] = newBuilder[B]
    for(x <- this) b += f(x)
    b.result.asInstanceOf[That]
  }

  override def flatMap[@sp B >: A, @sp That](f: (A) => GenTraversableOnce[B])
                                            (implicit bf: CanBuildFrom[Vector[A], B, That]): That = {
    def builder: VectorBuilder[B] = newBuilder[B] // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
    val b: VectorBuilder[B] = builder // Is mapping and then building the structure one at a time faster than first allocating space and then mapping
    for (x <- this) b ++= f(x).seq // Sugared form of foreach
    b.result.asInstanceOf[That]
  }

  override def filter(p: (A) => Boolean): Vector[A] = {
    val b: VectorBuilder[A] = newBuilder[A]
    for (x <- this)
      if (p(x)) b += x // Sugared form of foreach
    b.result
  }

  override def filterNot(p: (A) => Boolean): Vector[A] = {
    val b: VectorBuilder[A] = newBuilder[A]
    for (x <- this) if (!p(x)) b += x // Sugared form of foreach
    b.result
  }

  def collect[@sp B >: A, @sp That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Vector[A], B, That], ct: ClassTag[B]): That = {
    val b: VectorBuilder[B] = newBuilder[B]
    foreach(pf.runWith(b += _)) // Understand what a partial function
    b.result.asInstanceOf[That]
  }

  override def partition(p: (A) => Boolean): (Vector[A], Vector[A]) = {
    val l: VectorBuilder[A] = newBuilder[A]
    val r: VectorBuilder[A] = newBuilder[A]
    for (x <- this) (if (p(x)) l else r) += x // Sugared form of foreach
    (l.result, r.result)
  }

  def groupBy[@sp K](f: (A) => K)(implicit ct: ClassTag[K]): Map[K, Vector[A]] =  { // TODO Convert to vectorbuilder
    val m: mutable.Map[K, VectorBuilder[A]] = mutable.Map.empty
    for (elem <- this) {
      val key: K = f(elem)
      val builder: VectorBuilder[A] = m.getOrElseUpdate(key, newBuilder[A])
      builder += elem
    }
    val b: mutable.Builder[(K, Vector[A]), Map[K, Vector[A]]] = immutable.Map.newBuilder[K, Vector[A]]
    for ((k, v) <- m)
      b += ((k, v.result))

    b.result
  }

  override def scan[@sp B >: A, That](z: B)(op: (B, B) => B)
                                     (implicit cbf: CanBuildFrom[Vector[A], B, That]): That = scanLeft(z)(op)

  override def scanLeft[@sp B >: A, That](z: B)(op: (B, A) => B)
                                         (implicit bf: CanBuildFrom[Vector[A], B, That]): That = {
    val b: VectorBuilder[B] = newBuilder[B]
    b.sizeHint(this, 1)
    var acc: B = z
    b += acc
    for (x <- this) { acc = op(acc, x); b += acc } // Sugared form of foreach
    b.result.asInstanceOf[That]
  }

  override def scanRight[@sp B >: A, That](z: B)(op: (A, B) => B)
                                          (implicit bf: CanBuildFrom[Vector[A], B, That]): That = {
    var scanned = List(z) // TODO Maybe another data structure would be useful here and if the two for loops can be reduced to one
    var acc: B = z
    for (x <- reversed) { // Sugared form of foreach
      acc = op(x, acc)
      scanned ::= acc
    }
    val b: VectorBuilder[B] = newBuilder[B]
    for (elem <- scanned) b += elem // Sugared form of foreach
    b.result.asInstanceOf[That]
  }

  override def headOption: Option[A] = if (isEmpty) None else Some(head)

  override def tail: Vector[A] = if (isEmpty) throw new UnsupportedOperationException("empty.tail") else this.drop(1)

  override def last: A = {
    if (this.endIndex != 0)
    this.apply(this.size - 1)
  else
    throw new UnsupportedOperationException("empty.last")
  }

  override def lastOption: Option[A] = if (isEmpty) None else Some(last)

  override def init: Vector[A] = {
    if (this.endIndex != 0)
    dropRight(1)
  else
    throw new UnsupportedOperationException("empty.init")
  }

  /*Methods from TransferableLike */

  override def dropWhile(p: (A) => Boolean): Vector[A] = {
    val b: VectorBuilder[A] = newBuilder[A]
    var go: Boolean = false
    for (x <- this) {
      if (!go && !p(x)) go = true
      if (go) b += x
    }
    b.result
  }

  override def span(p: (A) => Boolean): (Vector[A], Vector[A]) = {
    val l = newBuilder
    val r = newBuilder
    val toLeft: Boolean = true
    for (x <- this) (if (toLeft && p(x)) l else r) += x
    (l.result, r.result)
  }

  override def splitAt(n: Int): (Vector[A], Vector[A]) = scala.Tuple2(take(n), drop(n))

  private def iterateUntilEmpty(f: Iterable[A @uncheckedVariance] => Iterable[A @uncheckedVariance]): Iterator[Vector[A]] = {
    val it = Iterator.iterate(thisCollection)(f) takeWhile (x => x.nonEmpty)
    it ++ Iterator(Nil) map (x => (newBuilder ++= x).result)
  }

  override def tails: Iterator[Vector[A]] = iterateUntilEmpty(_.tail)

  override def inits: Iterator[Vector[A]] = iterateUntilEmpty(_.init)

  def to[Vector[_]]: Immutable.Vector[A] = {
    val b: VectorBuilder[A] = newBuilder[A]
    b.sizeHint(this)
    b ++= thisCollection
    b.result
  }

  override def toString(): String = mkString(stringPrefix + "(", ", ", ")")

  override def stringPrefix: String =  {
    /* This method is written in a style that avoids calling `String.split()`
     * as well as methods of java.lang.Character that require the Unicode
     * database information. This is mostly important for Scala.js, so that
     * using the collection library does automatically bring java.util.regex.*
     * and the Unicode database in the generated code.
     *
     * This algorithm has the additional benefit that it won't allocate
     * anything except the result String in the common case, where the class
     * is not an inner class (i.e., when the result contains no '.').
     */
    val fqn = repr.getClass.getName
    var pos: Int = fqn.length - 1

    // Skip trailing $'s
    while (pos != -1 && fqn.charAt(pos) == '$') {
      pos -= 1
    }
    if (pos == -1 || fqn.charAt(pos) == '.') {
      return ""
    }

    var result: String = ""
    while (true) {
      // Invariant: if we enter the loop, there is a non-empty part

      // Look for the beginning of the part, remembering where was the last non-digit
      val partEnd = pos + 1
      while (pos != -1 && fqn.charAt(pos) <= '9' && fqn.charAt(pos) >= '0') {
        pos -= 1
      }
      val lastNonDigit = pos
      while (pos != -1 && fqn.charAt(pos) != '$' && fqn.charAt(pos) != '.') {
        pos -= 1
      }
      val partStart = pos + 1

      // A non-last part which contains only digits marks a method-local part -> drop the prefix
      if (pos == lastNonDigit && partEnd != fqn.length) {
        return result
      }

      // Skip to the next part, and determine whether we are the end
      while (pos != -1 && fqn.charAt(pos) == '$') {
        pos -= 1
      }
      val atEnd = pos == -1 || fqn.charAt(pos) == '.'

      // Handle the actual content of the part (we ignore parts that are likely synthetic)
      def isPartLikelySynthetic = {
        val firstChar = fqn.charAt(partStart)
        (firstChar > 'Z' && firstChar < 0x7f) || (firstChar < 'A')
      }
      if (atEnd || !isPartLikelySynthetic) {
        val part = fqn.substring(partStart, partEnd)
        result = if (result.isEmpty) part else part + '.' + result
        if (atEnd)
          return result
      }
    }

    // dead code
    result
  }

  /*Methods from Iterable */

  override def companion: scala.collection.generic.GenericCompanion[Vector] = Vector // TODO NOT SURE

  //override def seq: Iterable[A] = super.seq // TODO NOT SURE


  /*Methods from IterableLike */

  protected[this] override def thisCollection: Iterable[A] = this.asInstanceOf[Iterable[A]]

  protected[this] override def toCollection(repr: Vector[A]): Iterable[A] = repr.asInstanceOf[Iterable[A]]

  override def forall(p: (A) => Boolean): Boolean = iterator.forall(p)

  override def exists(p: (A) => Boolean): Boolean = iterator.exists(p)

  override def find(p: (A) => Boolean): Option[A] = iterator.find(p)

  override def isEmpty: Boolean = this.endIndex == 0

  override def foldRight[B](z: B)(op: (A, B) => B): B = iterator.foldRight(z)(op)

  override def reduceRight[B >: A](op: (A, B) => B): B = iterator.reduceRight(op)

  //override def toIterable: Iterable[A] = iterator.toIterable

  override def head: A = {
    if (this.endIndex != 0)
      apply(0)
    else
      throw new UnsupportedOperationException("empty.head")
  }

  override def slice(from: Int, until: Int): Vector[A] = take(until).drop(from)

  override def take(n: Int): Vector[A] = {
    if (n <= 0)
      Vector.empty
    else if (n < endIndex)
      takeFront0(n)
    else
      this
  }

  override def drop(n: Int): Vector[A] = {
    if (n <= 0)
      this
    else if (n.<(endIndex))
      dropFront0(n)
    else
      Vector.empty
  }

  override def takeWhile(p: (A) => Boolean): Vector[A] = {
    val b: VectorBuilder[A] = newBuilder
    val it: VectorIterator[A] = iterator
    while (it.hasNext) {
      val x = it.next()
      if (!p(x)) return b.result
      b += x
    }
    b.result
  }

  override def grouped(size: Int): Iterator[Vector[A]] =
    for (xs <- iterator grouped size) yield {
    val b: VectorBuilder[A] = newBuilder
    b ++= xs
    b.result
  }

  override def sliding(size: Int): Iterator[Vector[A]] = sliding(size, 1)

  override def sliding(size: Int, step: Int): Iterator[Vector[A]] =
    for (xs <- iterator.sliding(size, step)) yield {
    val b: VectorBuilder[A] = newBuilder
    b ++= xs
    b.result
  }

  override def takeRight(n: Int): Vector[A] = {
    if (n <= 0)
      Vector.empty
    else if (n < endIndex)
      dropFront0(endIndex - n)
    else
      this
  }

  override def dropRight(n: Int): Vector[A] = {
    if (n <= 0)
      this
    else if (n < endIndex)
      takeFront0(endIndex - n)
    else
      Vector.empty
  }

  override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Vector[A], (A1, B), That]): That = {
    val b: mutable.Builder[(A1, B), That] = bf(repr)
    val these: VectorIterator[A] = this.iterator
    val those: Iterator[B] = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next(), those.next()))
    b.result
  }

  override def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Vector[A], (A1, B), That]): That = {
    val b: mutable.Builder[(A1, B), That] = bf(repr)
    val these: VectorIterator[A] = this.iterator
    val those: Iterator[B] = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next(), those.next()))
    while (these.hasNext)
      b += ((these.next(), thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next()))
    b.result()
  }
  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Vector[A], (A1, Int), That]): That = {
    val b: mutable.Builder[(A1, Int), That] = bf(repr)
    var i: Int = 0
    for (x <- this) {
      b += ((x, i))
      i += 1
    }
    b.result
  }

  override def sameElements[@sp B >: A](that: GenIterable[B]): Boolean = {
    val these: VectorIterator[A] = this.iterator
    val those: Iterator[B] = that.iterator
    while (these.hasNext && those.hasNext)
      if (these.next != those.next)
        return false

    !these.hasNext && !those.hasNext
  }

  override def toStream: Stream[A] = iterator.toStream // TODO use fs2

  override def view(from: Int, until: Int): IterableView[A, Vector[A]] = view.slice(from, until)

  /*Methods from GenericTraversableTemplate */

  override def genericBuilder[@sp B]: mutable.Builder[B, Iterable[B]] = companion.newBuilder[B]

  private def sequential: TraversableOnce[A] = this.asInstanceOf[GenTraversableOnce[A]].seq

  override def unzip[@sp A1, @sp A2](implicit asPair: A => (A1, A2)): (Vector[A1], Vector[A2]) = {
    val b1 = newBuilder[A1]
    val b2 = newBuilder[A2]
    for (xy <- sequential) {
      val (x, y) = asPair(xy)
      b1 += x
      b2 += y
    }
    (b1.result, b2.result)
  }

  override def flatten[B](implicit asTraversable: (A) => GenTraversableOnce[B]): Iterable[B] = {
    val b: mutable.Builder[B, Iterable[B]] = newBuilder[B]
    for (xs <- sequential)
      b ++= asTraversable(xs).seq
    b.result
  }

  override def transpose[@sp B](implicit asTraversable: A => GenTraversableOnce[B]): Vector[Vector[B] @uncheckedVariance] = {
    if (isEmpty)
      return newBuilder[Vector[B]].result()

    def fail: Nothing = throw new IllegalArgumentException("transpose requires all collections have the same size")

    val headSize: Int = asTraversable(head).size
    val bs: IndexedSeq[mutable.Builder[B, Vector[B]]] = IndexedSeq.fill(headSize)(newBuilder[B])
    for (xs <- sequential) {
      var i: Int = 0
      for (x <- asTraversable(xs).seq) {
        if (i >= headSize) fail
        bs(i) += x
        i += 1
      }
      if (i != headSize)
        fail
    }
    val bb: VectorBuilder[Vector[B]] = newBuilder[Vector[B]]
    for (b <- bs) bb += b.result
    bb.result
  }

  override def count(p: (A) => Boolean): Int = {
    var count = 0
    for (x <- this)
      if (p(x)) count += 1
    count
  }

  override def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  override def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result: B = z
    this foreach (x => result = op(result, x))
    result
  }

  override def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first = true
    var acc: B = 0.asInstanceOf[B]

    for (x <- self) {
      if (first) {
        acc = x
        first = false
      }
      else acc = op(acc, x)
    }
    acc
  }

  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = if (isEmpty) None else Some(reduceLeft(op))

  override def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = if (isEmpty) None else Some(reduceRight(op))

  override def reduce[A1 >: A](op: (A1, A1) => A1): A1 = reduceLeft(op)

  override def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1] = reduceLeftOption(op)

  override def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)

  override def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)

  override def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  override def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  override def min[B >: A](implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")

    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  }

  override def max[B >: A](implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")

    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  }


  override def maxBy[B](f: (A) => B)(implicit cmp: Ordering[B]): A = {
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

  override def minBy[B](f: (A) => B)(implicit cmp: Ordering[B]): A = {
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

  override def toArray[@sp B >: A](implicit ev: ClassTag[B]): Array[B] = {
    if (isTraversableAgain) {
      val result = new Array[B](size)
      copyToArray(result, 0)
      result
    }
    else toBuffer.toArray
  }

  override def toMap[T, U](implicit ev: <:<[A, (T, U)]): Map[T, U] = {
    val b = immutable.Map.newBuilder[T, U]
    for (x <- self)
      b += x
    b.result()
  }

  def toTraversable: Traversable[A]

  override def toList: List[A] = to[List]

  override def toIterable: Iterable[A] = toStream

  override def toSeq: Seq[A] = toStream

  override def toIndexedSeq: immutable.IndexedSeq[A] = to[immutable.IndexedSeq]

  override def toBuffer[B >: A]: mutable.Buffer[B] = to[ArrayBuffer].asInstanceOf[mutable.Buffer[B]]

  override def toSet[B >: A]: immutable.Set[B] = to[immutable.Set].asInstanceOf[immutable.Set[B]]


  /*Methods from JavaLang*/

  override def hashCode(): Int

  override def clone(): AnyRef

  /* Helper Functions*/

  private[Immutable] def concatenate[@sp B >: A](currentSize: Int,
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
          if (that.display2.!=(null))
            d2 = that.display2

          if (d2 == null)
            d1 = that.display1
          else
            d1 = d2(0).asInstanceOf[Node]
          if (d1.==(null))
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
        if (that.focus.&(-32) == 0) {
          d5 = that.display5
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]
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
        if (that.focus.&(-32) == 0) {
          d6 = that.display6
          d5 = that.display5
          d4 = that.display4
          d3 = that.display3
          d2 = that.display2
          d1 = that.display1
          d0 = that.display0.asInstanceOf[Leaf]
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

  private def rebalanced[@sp B >: A : ClassTag](displayLeft: Node,
                                     concat: Node,
                                     displayRight: Node,
                                     currentDepth: Int): Node = {
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

    val top: Node = new Node(branching >> 10 + (if((branching & 1 << 10 - 1) == 0) 1 else 2))
    var mid: Node = new Node(if((branching >> 10) == 0) (branching + 31) >> 5 + 1 else 33)

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
              spire.math.min(branching -(iTop << 10) - (iMid << 5), 32)
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

  private def rebalancedLeafs[@sp B >: A](displayLeft: Array[B],
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

  private def computeBranching[@sp B >: A](displayLeft: Node,
                                           concat: Array[B],
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

  private[Immutable] def append[@sp B >: A: ClassTag](elem: B, _endIndex: Int): Unit = {
    if (focusStart.+(focus).^(_endIndex - 1) >= 32)
      normalizeAndFocusOn(_endIndex - 1)

    val elemIndexInBlock = _endIndex.-(focusStart).&(31)
    if (elemIndexInBlock != 0)
      appendOnCurrentBlock(elem, elemIndexInBlock)
    else
      appendBackNewBlock(elem, elemIndexInBlock)
  }

  private def appendOnCurrentBlock[@sp B >: A](elem: B,
                                               elemIndexInBlock: Int): Unit = {
    focusEnd = endIndex
    val d0 = new Leaf(elemIndexInBlock.+(1))
    System.arraycopy(display0, 0, d0, 0, elemIndexInBlock)
    d0.update(elemIndexInBlock, elem.asInstanceOf[A])
    display0 = d0
    makeTransientIfNeeded()
  }

  private def appendBackNewBlock[@sp B >: A](elem: B,
                                             elemIndexInBlock: Int)(implicit m: ClassTag[B]): Unit = {
    val oldDepth = depth
    val newRelaxedIndex = endIndex.-(1).-(focusStart).+(focusRelax)
    val focusJoined = focus.|(focusRelax)
    val xor = newRelaxedIndex.^(focusJoined)
    val _transient = transient
    setupNewBlockInNextBranch(xor, _transient)(m.asInstanceOf[ClassTag[A]])
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
      initFocus(endIndex.-(1), endIndex.-(1), endIndex, 1, newRelaxedIndex.&(-32))
    display0.update(elemIndexInBlock, elem.asInstanceOf[A])
    transient = true
  }

  private[Immutable] def makeTransientIfNeeded(): Unit = {
    val _depth = depth
    if (_depth > 1 && transient.`unary_!`) {
      copyDisplaysAndNullFocusedBranch(_depth, focus.|(focusRelax))
      transient = true
    }
  }

  private def createSingletonVector[@sp B >: A : ClassTag](elem: B): Vector[B] = {
    val resultVector = new Vector[B](1)
    resultVector.initSingleton(elem)
    resultVector
  }

  private[Immutable] def normalizeAndFocusOn(index: Int): Unit = {
    if (transient) {
      normalize(depth)
      transient = false
    }
    focusOn(index)
  }

  private[Immutable] def prepend[@sp B >: A: ClassTag](elem: B): Unit = {

    if (focusStart != 0 || (focus & -32) != 0)
      normalizeAndFocusOn(0)

    val d0 = display0
    if (d0.length < 32)
      prependOnCurrentBlock(elem, d0.asInstanceOf[Array[B]])
    else
      prependFrontNewBlock(elem)
  }

  private def prependOnCurrentBlock[@sp B >: A](elem: B,
                                                oldD0: Array[B]): Unit = {
    val newLen = oldD0.length + 1
    focusEnd = newLen
    val newD0 = new Leaf(newLen)
    newD0.update(0, elem.asInstanceOf[A])
    System.arraycopy(oldD0, 0, newD0, 1, newLen.-(1))
    display0 = newD0
    makeTransientIfNeeded()
  }
  private def prependFrontNewBlock[@sp B >: A](elem: B)(implicit m: ClassTag[B]): Unit = {
    var currentDepth = focusDepth
    if (currentDepth == 1)
      currentDepth += 1

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
    while ((display != null).&&(display.length.==(33))) {
      currentDepth += 1
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
    setupNewBlockInInitBranch(currentDepth, _transient)(m.asInstanceOf[ClassTag[A]])
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
    display0.update(0, elem.asInstanceOf[A])
    transient = true
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

  private def dropFront0(n: Int): Vector[A] = {

    if (transient) {
      normalize(depth)
      transient = false
    }

    val vec: Vector[A] = new Vector[A](this.endIndex - n)

    vec.initWithFocusFrom(this)

    if (vec.depth > 1) {
      vec.focusOn(n)
      val cutIndex: Int = vec.focus | vec.focusRelax
      val d0Start: Int = cutIndex & 31
      if (d0Start != 0){
        val d0len: Int = vec.display0.length - d0Start
        val d0: Leaf = new Leaf(d0len)
        System.arraycopy(vec.display0, d0Start, d0, 0, d0len)
        vec.display0 = d0
      }

      vec.cleanTopDrop(cutIndex)

      if (vec.depth > 1) {
        var i = 2
        var display = vec.display1
        while (i <= vec.depth) {
          val splitStart = cutIndex >> 5 * (i - 1) & 31
          val newLen = display.length - splitStart - 1
          val newDisplay = new Node(newLen + 1)
          System.arraycopy(display, splitStart + 1, newDisplay, 1, newLen - 1)
          i match {
            case 2 =>
              newDisplay.update(0, vec.display0)
              vec.display1 = withComputedSizes(newDisplay, 2)
              display = vec.display2
            case 3 =>
              newDisplay.update(0, vec.display1)
              vec.display2 = withComputedSizes(newDisplay, 3)
              display = vec.display3
            case 4 =>
              newDisplay.update(0, vec.display2)
              vec.display3 = withComputedSizes(newDisplay, 4)
              display = vec.display4
            case 5 =>
              newDisplay.update(0, vec.display3)
              vec.display4 = withComputedSizes(newDisplay, 5)
              display = vec.display5
            case 6 =>
              newDisplay.update(0, vec.display4)
              vec.display5 = withComputedSizes(newDisplay, 6)
              display = vec.display6
            case 7 =>
              newDisplay.update(0, vec.display5)
              vec.display6 = withComputedSizes(newDisplay, 7)
          }
          i += 1
        }
      }

      vec.initFocus(0, 0, vec.display0.length, 1, 0)
    } else {

      val newLen = vec.display0.length - n
      val d0 = new Leaf(newLen)
      System.arraycopy(vec.display0, n, d0, 0, newLen)
      vec.display0 = d0
      vec.initFocus(0, 0, newLen, 1, 0)
    }
    vec
  }


}

