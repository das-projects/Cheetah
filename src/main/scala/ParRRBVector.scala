package Cheetah
package collection
package immutable

import scala.collection.generic.{CanCombineFrom, GenericParTemplate, ParFactory}
import scala.collection.parallel.{Combiner, ParSeqLike, SeqSplitter}
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable.ParSeq

class ParRRBVector[+A](vector: RRBVector[A])
    extends ParSeq[A]
    with GenericParTemplate[A, ParRRBVector]
    with ParSeqLike[A, ParRRBVector[A], RRBVector[A]]
    with Serializable {

  override def companion = ParRRBVector

  def this() = {
    this(RRBVector.empty[A])
    ()
  }

  def apply(idx: Int) = vector.apply(idx)

  def length = vector.length

  def splitter: SeqSplitter[A] = {
    val pit = new ParRRBVectorIterator(0, vector.length)
    pit.initIteratorFrom(vector)
    pit
  }

  override def seq: RRBVector[A] = vector

  override def toVector: Vector[A] = vector.toVector

  class ParRRBVectorIterator(_start: Int, _end: Int)
      extends RRBVectorIterator[A](_start, _end)
      with SeqSplitter[A] {
    final override def remaining: Int = super.remaining
    def dup: SeqSplitter[A] = {
      val pit =
        new ParRRBVectorIterator(_end.-(remaining), _end)
      pit.initIteratorFrom(this)
      pit
    }
    def split: Seq[ParRRBVectorIterator] = {
      val rem = remaining
      if (rem.>=(2)) {
        val _half = rem./(2)
        val _splitModulo =
          if (rem.<=(32))
            1
          else if (rem.<=(1024))
            32
          else if (rem.<=(32768))
            1024
          else if (rem.<=(1048576))
            32768
          else if (rem.<=(33554432))
            1048576
          else if (rem.<=(1073741824))
            33554432
          else
            1073741824
        val _halfAdjusted =
          if (_half.>(_splitModulo))
            _half.-(_half.%(_splitModulo))
          else if (_splitModulo.<(_end))
            _splitModulo
          else
            _half
        psplit(_halfAdjusted, rem.-(_halfAdjusted))
      } else
        Seq(this)
    }
    def psplit(sizes: Int*): Seq[ParRRBVectorIterator] = {
      val splitted = new ArrayBuffer[ParRRBVectorIterator]()
      var currentPos = _end.-(remaining)
      sizes.foreach(((sz) => {
        val pit = new ParRRBVectorIterator(currentPos, currentPos.+(sz))
        pit.initIteratorFrom(this)
        splitted.+=(pit)
        currentPos.+=(sz)
      }))
      splitted
    }
  }
}

object ParRRBVector extends ParFactory[ParRRBVector] {
  implicit def canBuildFrom[A]: CanCombineFrom[Coll, A, ParRRBVector[A]] =
    new GenericCanCombineFrom[A]()
  def newBuilder[A]: Combiner[A, ParRRBVector[A]] =
    newCombiner[A]
  def newCombiner[A]: Combiner[A, ParRRBVector[A]] =
    new ParRRBVectorCombinator[A]()
}

private[immutable] class ParRRBVectorCombinator[A]
    extends Combiner[A, ParRRBVector[A]] {
  val builder: RRBVectorBuilder[A] =
    new RRBVectorBuilder[A]()
  override def size = builder.endIndex
  override def result() =
    new ParRRBVector[A](builder.result())
  override def clear() = builder.clear()
  override def +=(elem: A) = {
    builder.+=(elem)
    this
  }
  override def ++=(xs: TraversableOnce[A]) = {
    builder.++=(xs)
    this
  }
  def combine[B <: A, NewTo >: ParRRBVector[A]](
      other: Combiner[B, NewTo]) =
    if (other.eq(this))
      this
    else {
      val newCombiner = new ParRRBVectorCombinator[A]()
      newCombiner.++=(this.builder.result())
      newCombiner.++=(
        other
          .asInstanceOf[ParRRBVectorCombinator[A]]
          .builder
          .result())
      newCombiner
    }
}
