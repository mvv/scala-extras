package scalax.data

import scala.collection
import collection.LinearSeqLike
import collection.generic._
import collection.immutable.LinearSeq
import collection.mutable.Builder

class FingerBuilder[A] extends Builder[A, (FingerTree[A], Int)] {
  private var tree = FingerTree.empty[A] 
  private var size = 0

  override def ++=(elems: TraversableOnce[A]) = {
    elems match {
      case xs: FingerSeq[_] =>
        tree = tree.append(xs.tree)
        size += xs.size
      case _ =>
        super.++=(elems)
    }
    this
  }

  def +=(elem: A) = {
    tree = tree.append(elem)
    size += 1
    this
  }

  def clear() {
    tree = FingerTree.empty
    size = 0
  }

  def result = (tree, size)
}

class FingerSeq[+A] private(val tree: FingerTree[A], override val length: Int)
        extends LinearSeq[A]
           with GenericTraversableTemplate[A, FingerSeq]
           with LinearSeqLike[A, FingerSeq[A]] {
  override def companion: GenericCompanion[FingerSeq] = FingerSeq

  override def iterator: Iterator[A] = tree.iterator
  override def reverseIterator: Iterator[A] = tree.reverseIterator

  def apply(i: Int): A = {
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException()
    iterator.drop(i).next
  }

  def prepend[B >: A](x: B) = new FingerSeq(tree.prepend(x), length + 1)
  def append[B >: A](x: B) = new FingerSeq(tree.append(x), length + 1)
  override def head = tree.head
  override def headOption = tree.headOption
  override def tail = new FingerSeq(tree.tail, length - 1)
  override def last = tree.last
  override def lastOption = tree.lastOption
  override def init = new FingerSeq(tree.init, length - 1)
  def map[B](f: A => B) = new FingerSeq(tree.map(f), length)
}

object FingerSeq extends SeqFactory[FingerSeq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, FingerSeq[A]] =
    new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, FingerSeq[A]] =
    new FingerBuilder[A] mapResult { case (tree, size) =>
      new FingerSeq(tree, size)
    }
  override def empty[A]: FingerSeq[A] = new FingerSeq(FingerTree.Empty, 0)
}

