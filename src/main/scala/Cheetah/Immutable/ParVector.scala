package Cheetah.Immutable

import scala.collection.{GenIterable, GenSeq, GenTraversableOnce}
import scala.collection.generic.{CanBuildFrom, CanCombineFrom, GenericParTemplate, ParFactory}
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.{Combiner, ParSeqLike, SeqSplitter}
import scala.collection.parallel.immutable.ParSeq
import scala.reflect.ClassTag
import scala.{specialized => sp}
import scala.{Vector => Vec}

class ParVector[@sp +A](vector: Vector[A])
  extends ParSeq[A]
    with GenericParTemplate[A, ParVector]
    with ParSeqLike[A, ParVector[A], Vector[A]]
    with Serializable {

  override def reverseMap[B, That](f: (A) => B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[ParVector[A], (A1, B), That]) = ???

  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[ParVector[A], (A1, Int), That]) = ???

  override def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[ParVector[A], (A1, B), That]) = ???

  override def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def flatMap[B, That](f: (A) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[ParVector[A], B, That]) = ???

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

  //override def toVector: Vector[A] = vector.toVector

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
      sizes.foreach((sz) => {
        val pit = new ParVectorIterator(currentPos, currentPos.+(sz))
        pit.initIteratorFrom(this)
        splitted += pit
        currentPos += sz
      })
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

private[Immutable] class ParVectorCombinator[A : ClassTag]
  extends Combiner[A, ParVector[A]] {
  val builder: VectorBuilder[A] =
    new VectorBuilder[A]()
  override def size: Int = builder.endIndex
  override def result() =
    new ParVector[A](builder.result())
  override def clear(): Unit = builder.clear()

  override def +=(elem: A): ParVectorCombinator[A] = {
    builder += elem
    this
  }
  override def ++=(xs: TraversableOnce[A]): ParVectorCombinator.this.type = {
    builder ++= xs
    this
  }
  def combine[B <: A, NewTo >: ParVector[A]](
                                                 other: Combiner[B, NewTo]) =
    if (other.eq(this))
      this
    else {
      val newCombiner = new ParVectorCombinator[A]()
      newCombiner ++= this.builder.result()
      newCombiner ++=
        other
          .asInstanceOf[ParVectorCombinator[A]]
          .builder
          .result()
      newCombiner
    }
}
