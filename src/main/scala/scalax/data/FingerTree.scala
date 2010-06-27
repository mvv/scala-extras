package scalax.data

trait Power[+A] extends Product {
  def iterator: Iterator[A] = productIterator.asInstanceOf[Iterator[A]]
  def reverseIterator = new Iterator[A] {
    private var i = productArity
    def hasNext = i > 0
    def next =
      if (i > 0) {
        i -= 1
        productElement(i).asInstanceOf[A]
      } else
        throw new IllegalStateException()
  }
  def toIterator: Iterator[A] = iterator
  def map[B](f: A => B): Power[B]
  def foldLeft[C](z: C)(f: (C, A) => C): C = iterator.foldLeft(z)(f)
  def foldRight[C](z: C)(f: (A, C) => C): C =
    reverseIterator.foldLeft(z) { case (acc, e) => f(e, acc) }
}

trait PositivePower[+A] extends Power[A] {
  def head: A = productElement(0).asInstanceOf[A]
  def last: A = productElement(productArity - 1).asInstanceOf[A]
}

trait Power1[+A] extends PositivePower[A] with Product1[A] {
  override def head = _1
  override def last = _1
  override def iterator = Iterator.single(_1)
  override def reverseIterator = Iterator.single(_1)
}
trait Power2[+A] extends PositivePower[A] with Product2[A, A] {
  override def head = _1
  override def last = _2
}
trait Power3[+A] extends PositivePower[A] with Product3[A, A, A] {
  override def head = _1
  override def last = _3
}
trait Power4[+A] extends PositivePower[A] with Product4[A, A, A, A] {
  override def head = _1
  override def last = _4
}

sealed trait Node23[+A] extends PositivePower[A] {
  def map[B](f: A => B): Node23[B]
}
final case class Node2[A](_1: A, _2: A) extends Power2[A] with Node23[A] {
  def map[B](f: A => B): Node2[B] = Node2(f(_1), f(_2))
}
final case class Node3[A](_1: A, _2: A, _3: A) extends Power3[A] with Node23[A] {
  def map[B](f: A => B): Node3[B] = Node3(f(_1), f(_2), f(_3))
}

sealed trait Digits[+A] extends PositivePower[A] {
  def ++[B >: A](xs: Digits[B]): Digits[Node23[B]]
  def +++[B >: A](xs: Digits[B], ys: Digits[B]): Digits[Node23[B]]
  def map[B](f: A => B): Digits[B]
}
object Digits {
  final case class One[A](_1: A) extends Power1[A] with Digits[A] {
    def ++[B >: A](xs: Digits[B]) = xs match {
      case One(x1) => One(Node2(_1, x1))
      case Two(x1, x2) => One(Node3(_1, x1, x2))
      case Three(x1, x2, x3) => Two(Node2(_1, x1), Node2(x2, x3))
      case Four(x1, x2, x3, x4) => Two(Node3(_1, x1, x2), Node2(x3, x4))
    }
    def +++[B >: A](xs: Digits[B], ys: Digits[B]) = xs match {
      case One(x1) => ys match {
        case One(y1) => One(Node3(_1, x1, y1))
        case Two(y1, y2) => Two(Node2(_1, x1), Node2(y1, y2))
        case Three(y1, y2, y3) => Two(Node3(_1, x1, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) => Two(Node3(_1, x1, y1), Node3(y2, y3, y4))
      }
      case Two(x1, x2) => ys match {
        case One(y1) => Two(Node2(_1, x1), Node2(x2, y1))
        case Two(y1, y2) => Two(Node3(_1, x1, x2), Node2(y1, y2))
        case Three(y1, y2, y3) => Two(Node3(_1, x1, x2), Node3(y1, y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, x1, x2), Node2(y1, y2), Node2(y3, y4))
      }
      case Three(x1, x2, x3) => ys match {
        case One(y1) => Two(Node3(_1, x1, x2), Node2(x3, y1))
        case Two(y1, y2) => Two(Node3(_1, x1, x2), Node3(x3, y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, x1, x2), Node2(x3, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, x1, x2), Node3(x3, y1, y2), Node2(y3, y4))
      }
      case Four(x1, x2, x3, x4) => ys match {
        case One(y1) => Two(Node3(_1, x1, x2), Node3(x3, x4, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, x1, x2), Node2(x3, x4), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, x1, x2), Node3(x3, x4, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, x1, x2), Node3(x3, x4, y1), Node3(y2, y3, y4))
      }
    }
    def map[B](f: A => B): One[B] = One(f(_1))
  }
  final case class Two[A](_1: A, _2: A) extends Power2[A] with Digits[A] {
    def ++[B >: A](xs: Digits[B]) = xs match {
      case One(x1) => One(Node3(_1, _2, x1))
      case Two(x1, x2) => Two(Node2(_1, _2), Node2(x1, x2))
      case Three(x1, x2, x3) => Two(Node3(_1, _2, x1), Node2(x2, x3))
      case Four(x1, x2, x3, x4) => Two(Node3(_1, _2, x1), Node3(x2, x3, x4))
    }
    def +++[B >: A](xs: Digits[B], ys: Digits[B]) = xs match {
      case One(x1) => ys match {
        case One(y1) => Two(Node2(_1, _2), Node2(x1, y1))
        case Two(y1, y2) => Two(Node3(_1, _2, x1), Node2(y1, y2))
        case Three(y1, y2, y3) => Two(Node3(_1, _2, x1), Node3(y1, y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, _2, x1), Node2(y1, y2), Node2(y3, y4))
      }
      case Two(x1, x2) => ys match {
        case One(y1) => Two(Node3(_1, _2, x1), Node2(x2, y1))
        case Two(y1, y2) => Two(Node3(_1, _2, x1), Node3(x2, y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, x1), Node2(x1, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, _2, x1), Node3(x2, y1, y2), Node2(y3, y4))
      }
      case Three(x1, x2, x3) => ys match {
        case One(y1) => Two(Node3(_1, _2, x1), Node3(x2, x3, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, x1), Node2(x2, x3), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, x1), Node3(x2, x3, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, _2, x1), Node3(x2, x3, y1), Node3(y2, y3, y4))
      }
      case Four(x1, x2, x3, x4) => ys match {
        case One(y1) => Three(Node3(_1, _2, x1), Node2(x2, x3), Node2(x4, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, x1), Node3(x2, x3, x4), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, x1), Node3(x2, x3, x4), Node3(y1, y2, y3))
        case Four(y1, y2, y3, y4) =>
          Four(Node3(_1, _2, x1), Node3(x2, x3, x4), Node2(y1, y2), Node2(y3, y4))
      }
    }
    def map[B](f: A => B): Two[B] = Two(f(_1), f(_2))
  }
  final case class Three[A](_1: A, _2: A, _3: A) extends Power3[A] with Digits[A] {
    def ++[B >: A](xs: Digits[B]) = xs match {
      case One(x1) => Two(Node2(_1, _2), Node2(_3, x1))
      case Two(x1, x2) => Two(Node3(_1, _2, _3), Node2(x1, x2))
      case Three(x1, x2, x3) => Two(Node3(_1, _2, _3), Node3(x1, x2, x3))
      case Four(x1, x2, x3, x4) =>
        Three(Node3(_1, _2, _3), Node2(x1, x2), Node2(x3, x4))
    }
    def +++[B >: A](xs: Digits[B], ys: Digits[B]) = xs match {
      case One(x1) => ys match {
        case One(y1) => Two(Node3(_1, _2, _3), Node2(x1, y1))
        case Two(y1, y2) => Two(Node3(_1, _2, _3), Node3(x1, y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, _3), Node2(x1, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, _2, _3), Node3(x1, y1, y2), Node2(y3, y4))
      }
      case Two(x1, x2) => ys match {
        case One(y1) => Two(Node3(_1, _2, _3), Node3(x1, x2, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, _3), Node2(x1, x2), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, _3), Node3(x1, x2, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, _2, _3), Node3(x1, x2, y1), Node3(y2, y3, y4))
      }
      case Three(x1, x2, x3) => ys match {
        case One(y1) => Three(Node3(_1, _2, _3), Node2(x1, x2), Node2(x3, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, _3), Node3(x1, x2, x3), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, _3), Node3(x1, x2, x3), Node3(y1, y2, y3))
        case Four(y1, y2, y3, y4) =>
          Four(Node3(_1, _2, _3), Node3(x1, x2, x3), Node2(y1, y2), Node2(y3, y4))
      }
      case Four(x1, x2, x3, x4) => ys match {
        case One(y1) =>
          Three(Node3(_1, _2, _3), Node3(x1, x2, x3), Node2(x4, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, _3), Node3(x1, x2, x3), Node3(x4, y1, y2))
        case Three(y1, y2, y3) =>
          Four(Node3(_1, _2, _3), Node3(x1, x2, x3), Node2(x4, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Four(Node3(_1, _2, _3), Node3(x1, x2, x3), Node3(x4, y1, y2), Node2(y3, y4))
      }
    }
    def map[B](f: A => B): Three[B] = Three(f(_1), f(_2), f(_3))
  }
  final case class Four[A](_1: A, _2: A, _3: A, _4: A) extends Power4[A] with Digits[A] {
    def ++[B >: A](xs: Digits[B]) = xs match {
      case One(x1) => Two(Node3(_1, _2, _3), Node2(_4, x1))
      case Two(x1, x2) => Two(Node3(_1, _2, _3), Node3(_4, x1, x2))
      case Three(x1, x2, x3) =>
        Three(Node3(_1, _2, _3), Node2(_4, x1), Node2(x2, x3))
      case Four(x1, x2, x3, x4) =>
        Three(Node3(_1, _2, _3), Node3(_4, x1, x2), Node2(x3, x4))
    }
    def +++[B >: A](xs: Digits[B], ys: Digits[B]) = xs match {
      case One(x1) => ys match {
        case One(y1) => Two(Node3(_1, _2, _3), Node3(_4, x1, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, _3), Node2(_4, x1), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, _3), Node3(_4, x1, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Three(Node3(_1, _2, _3), Node3(_4, x1, y1), Node3(y2, y3, y4))
      }
      case Two(x1, x2) => ys match {
        case One(y1) => Three(Node3(_1, _2, _3), Node2(_4, x1), Node2(x2, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, _3), Node3(_4, x1, x2), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Three(Node3(_1, _2, _3), Node3(_4, x1, x2), Node3(y1, y2, y3))
        case Four(y1, y2, y3, y4) =>
          Four(Node3(_1, _2, _3), Node3(_4, x1, x2), Node2(y1, y2), Node2(y3, y4))
      }
      case Three(x1, x2, x3) => ys match {
        case One(y1) => Three(Node3(_1, _2, _3), Node3(_4, x1, x2), Node2(x3, y1))
        case Two(y1, y2) =>
          Three(Node3(_1, _2, _3), Node3(_4, x1, x2), Node3(x3, y1, y2))
        case Three(y1, y2, y3) =>
          Four(Node3(_1, _2, _3), Node3(_4, x1, x2), Node2(x3, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Four(Node3(_1, _2, _3), Node3(_4, x1, x2), Node3(x3, y1, y2), Node2(y3, y4))
      }
      case Four(x1, x2, x3, x4) => ys match {
        case One(y1) =>
          Three(Node3(_1, _2, _3), Node3(_4, x1, x2), Node3(x3, x4, y1))
        case Two(y1, y2) =>
          Four(Node3(_1, _2, _3), Node3(_4, x1, x2), Node2(x3, x4), Node2(y1, y2))
        case Three(y1, y2, y3) =>
          Four(Node3(_1, _2, _3), Node3(_4, x1, x2), Node3(x3, x4, y1), Node2(y2, y3))
        case Four(y1, y2, y3, y4) =>
          Four(Node3(_1, _2, _3), Node3(_4, x1, x2), Node3(x3, x4, y1), Node3(y2, y3, y4))
      }
    }
    def map[B](f: A => B): Four[B] = Four(f(_1), f(_2), f(_3), f(_4))
  }
}

sealed trait FingerTree[+A] {
  def prepend[B >: A](x: B): FingerTree[B]
  def append[B >: A](x: B): FingerTree[B]
  def prepend[B >: A](tree: FingerTree[B]): FingerTree[B]
  def append[B >: A](tree: FingerTree[B]): FingerTree[B]
  def head: A
  def headOption: Option[A]
  def last: A
  def lastOption: Option[A]
  def tail: FingerTree[A]
  def init: FingerTree[A]
  def iterator: Iterator[A]
  def reverseIterator: Iterator[A]
  def map[B](f: A => B): FingerTree[B]
}

object FingerTree {
  import Digits._

  def empty[A]: FingerTree[A] = Empty

  object Empty extends FingerTree[Nothing] {
    def prepend[B >: Nothing](x: B) = Single(x)
    def append[B >: Nothing](x: B) = Single(x)
    def prepend[B >: Nothing](tree: FingerTree[B]) = tree
    def append[B >: Nothing](tree: FingerTree[B]) = tree
    def head = throw new NoSuchElementException()
    def headOption = None
    def last = throw new NoSuchElementException()
    def lastOption = None
    def tail = throw new NoSuchElementException()
    def init = throw new NoSuchElementException()
    def iterator = Iterator.empty
    def reverseIterator = Iterator.empty
    def map[B](f: Nothing => B) = empty[B]
  }
  case class Single[A](value: A) extends FingerTree[A] {
    def prepend[B >: A](x: B) = Deep(One(x), Empty, One(value))
    def append[B >: A](x: B) = Deep(One(value), Empty, One(x))
    def prepend[B >: A](tree: FingerTree[B]) = tree.append(value)
    def append[B >: A](tree: FingerTree[B]) = tree.prepend(value)
    def head = value
    def headOption = Some(value)
    def last = value
    def lastOption = Some(value)
    def tail = Empty
    def init = Empty
    def iterator = Iterator.single(value)
    def reverseIterator = Iterator.single(value)
    def map[B](f: A => B) = Single(f(value))
  }
  case class Deep[A](left: Digits[A], child: FingerTree[Node23[A]], right: Digits[A]) extends FingerTree[A] {
    def prepend[B >: A](x: B) = left match {
      case One(x1) => Deep(Two(x, x1), child, right)
      case Two(x1, x2) => Deep(Three(x, x1, x2), child, right)
      case Three(x1, x2, x3) => Deep(Four(x, x1, x2, x3), child, right)
      case Four(x1, x2, x3, x4) =>
        Deep(Two(x, x1), child.prepend(Node3(x2, x3, x4)), right)
    }
    def append[B >: A](x: B) = right match {
      case One(x1) => Deep(left, child, Two(x1, x))
      case Two(x1, x2) => Deep(left, child, Three(x1, x2, x))
      case Three(x1, x2, x3) => Deep(left, child, Four(x1, x2, x3, x))
      case Four(x1, x2, x3, x4) =>
        Deep(left, child.append(Node3(x1, x2, x3)), Two(x4, x))
    }
    private def concat[T](leftTree: FingerTree[Node23[T]], digits: Digits[Node23[T]],
                          rightTree: FingerTree[Node23[T]]): FingerTree[Node23[T]] = (leftTree, rightTree) match {
      case (Empty, _) =>
        digits.foldRight(rightTree)((e, t) => t prepend e)
      case (Single(x), _) =>
        digits.foldRight(rightTree)((e, t) => t prepend e) prepend x
      case (_, Empty) =>
        digits.foldLeft(leftTree)((t, e) => t append e)
      case (_, Single(x)) =>
        digits.foldLeft(leftTree)((t, e) => t append e) append x
      case (Deep(ll, lc, lr), Deep(rl, rc, rr)) =>
        Deep(ll, concat(lc, lr.+++(digits, rl), rc), rr)
    }
    def prepend[B >: A](tree: FingerTree[B]) = tree match {
      case Empty => this
      case Single(x) => prepend(x)
      case Deep(tLeft, tChild, tRight) =>
        Deep(tLeft, concat(tChild, tRight ++ left, child), right)
    }
    def append[B >: A](tree: FingerTree[B]) = tree match {
      case Empty => this
      case Single(x) => append(x)
      case Deep(tLeft, tChild, tRight) =>
        Deep(left, concat(child, right ++ tLeft, tChild), tRight)
    }
    def head = left.head
    def headOption = Some(left.head)
    def last = right.last
    def lastOption = Some(right.last)
    def tail = left match {
      case Two(x1, x2) => Deep(One(x2), child, right)
      case Three(x1, x2, x3) => Deep(Two(x2, x3), child, right)
      case Four(x1, x2, x3, x4) => Deep(Three(x2, x3, x4), child, right)
      case One(_) => child match {
        case Empty => right match {
          case Four(x1, x2, x3, x4) => Deep(Two(x1, x2), Empty, Two(x3, x4))
          case Three(x1, x2, x3) => Deep(Two(x1, x2), Empty, One(x3))
          case Two(x1, x2) => Deep(One(x1), Empty, One(x2))
          case One(x1) => Single(x1)
        }
        case _ => (child.head, child.tail) match {
          case (Node2(x1, x2), rest) => Deep(Two(x1, x2), rest, right)
          case (Node3(x1, x2, x3), rest) => Deep(Three(x1, x2, x3), rest, right)
        }
      }
    }
    def init = right match {
      case Two(x1, x2) => Deep(left, child, One(x1))
      case Three(x1, x2, x3) => Deep(left, child, Two(x1, x2))
      case Four(x1, x2, x3, x4) => Deep(left, child, Three(x1, x2, x3))
      case One(_) => child match {
        case Empty => left match {
          case Four(x1, x2, x3, x4) => Deep(Two(x1, x2), Empty, Two(x3, x4))
          case Three(x1, x2, x3) => Deep(One(x1), Empty, Two(x2, x3))
          case Two(x1, x2) => Deep(One(x1), Empty, One(x2))
          case One(x1) => Single(x1)
        }
        case _ => (child.init, child.last) match {
          case (rest, Node2(x1, x2)) => Deep(left, rest, Two(x1, x2))
          case (rest, Node3(x1, x2, x3)) => Deep(left, rest, Three(x1, x2, x3))
        }
      }
    }
    def iterator =
      left.iterator ++
      child.iterator.map(_.iterator).flatten ++
      right.iterator
    def reverseIterator =
      right.reverseIterator ++
      child.reverseIterator.map(_.reverseIterator).flatten ++
      left.reverseIterator
    def map[B](f: A => B) = Deep(left.map(f), child.map(_.map(f)), right.map(f))
  }
}
