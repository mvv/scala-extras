package scalax.data

class WrappedIterator[+A](it: Iterator[A]) extends Iterator[A] {
  def hasNext = it.hasNext
  def next = it.next
}
