package Cheetah.Immutable

import scala.collection.generic.{CanCombineFrom, GenericParTemplate, ParFactory}
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.{Combiner, ParSeqLike, SeqSplitter}
import scala.collection.parallel.immutable.ParSeq
import scala.{specialized => sp}
import scala.{Vector => Vec}

class ParVector[@sp +A](vector: Vector[A])
  extends ParSeq[A]
    with GenericParTemplate[A, ParVector]
    with ParSeqLike[A, ParVector[A], Vector[A]]
    with Serializable {

  override def companion = ParVector

  def this() = {
    this(Vector.empty[A])
    ()
  }

  def apply(idx: Int) = vector.apply(idx)

  def length = vector.size

  def splitter: SeqSplitter[A] = {
    val pit = new ParVectorIterator(0, vector.size)
    pit.initIteratorFrom(vector)
    pit
  }

  override def seq: Vector[A] = vector

  override def toVector: Vector[A] = vector.toVector

  class ParVectorIterator(_start: Int, _end: Int)
    extends VectorIterator[A](_start, _end)
      with SeqSplitter[A] {
    final override def remaining: Int = super.remaining
    def dup: SeqSplitter[A] = {
      val pit =
        new ParVectorIterator(_end.-(remaining), _end)
      pit.initIteratorFrom(this)
      pit
    }
    def split: Seq[ParVectorIterator] = {
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
    def psplit(sizes: Int*): Seq[ParVectorIterator] = {
      val splitted = new ArrayBuffer[ParVectorIterator]()
      var currentPos = _end.-(remaining)
      sizes.foreach(((sz) => {
        val pit = new ParVectorIterator(currentPos, currentPos.+(sz))
        pit.initIteratorFrom(this)
        splitted.+=(pit)
        currentPos.+=(sz)
      }))
      splitted
    }
  }
}

object ParVector extends ParFactory[ParVector] {
  implicit def canBuildFrom[A]: CanCombineFrom[Coll, A, ParVector[A]] =
    new GenericCanCombineFrom[A]()
  def newBuilder[A]: Combiner[A, ParVector[A]] =
    newCombiner[A]
  def newCombiner[A]: Combiner[A, ParVector[A]] =
    new ParVectorCombinator[A]()
}

private[Immutable] class ParVectorCombinator[A]
  extends Combiner[A, ParVector[A]] {
  val builder: VectorBuilder[A] =
    new VectorBuilder[A]()
  override def size = builder.endIndex
  override def result() =
    new ParVector[A](builder.result())
  override def clear() = builder.clear()
  override def +=(elem: A) = {
    builder.+=(elem)
    this
  }
  override def ++=(xs: TraversableOnce[A]) = {
    builder.++=(xs)
    this
  }
  def combine[B <: A, NewTo >: ParVector[A]](
                                                 other: Combiner[B, NewTo]) =
    if (other.eq(this))
      this
    else {
      val newCombiner = new ParVectorCombinator[A]()
      newCombiner.++=(this.builder.result())
      newCombiner.++=(
        other
          .asInstanceOf[ParVectorCombinator[A]]
          .builder
          .result())
      newCombiner
    }
}
