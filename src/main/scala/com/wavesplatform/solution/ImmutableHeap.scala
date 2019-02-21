package com.wavesplatform.solution

/**
  * Immutable PriorityQueue in Scala
  * Most functions run in O(log n)
  * Developed in Scala version 2.7.7
  * Implementation: Leftist Heap (See "Purely Functional Data Structures")
  *
  * @author Masaki Hara (ackie.h.gmai@gmail.com)
  */
object SortedQueue {
  def apply[A]()(implicit ord: Ordering[A]): SortedQueue[A] = empty

  def apply[A](elem: A)(implicit ord: Ordering[A]): SortedQueue[A] = single(elem)

  def unapply[A](arg: SortedQueue[A]): Option[(A, SortedQueue[A])] = Some(arg.head, arg.tail)

  def empty[A](implicit ord: Ordering[A]): SortedQueue[A] = Empty[A]()

  def single[A](elem: A)(implicit ord: Ordering[A]): SortedQueue[A] = Tree[A](1, 1, elem, empty, empty)

  def merge[A](a: SortedQueue[A], b: SortedQueue[A])
    (implicit ord: Ordering[A]): SortedQueue[A] =
    a match {
      case Empty()                => b
      case Tree(_, _, ae, al, ar) => b match {
        case Empty()                => a
        case Tree(_, _, be, bl, br) =>
          if (ord.compare(ae, be) > 0)
            makeT(ae, al, merge(ar, b))
          else
            makeT(be, bl, merge(br, a))
      }
    }

  def makeT[A](head: A, a: SortedQueue[A], b: SortedQueue[A])
    (implicit ord: Ordering[A]): SortedQueue[A] =
    if (a.rank >= b.rank)
      Tree(a.length + b.length + 1, b.rank + 1, head, a, b)
    else
      Tree(a.length + b.length + 1, a.rank + 1, head, b, a)

}

sealed abstract class SortedQueue[A](implicit val ord: Ordering[A]) {
  protected def rank: Int

  def internalApply(idx: Int): A

  def length: Int

  def isEmpty: Boolean = length == 0

  def head: A

  def headOption: Option[A] = if (length == 0) None else Some(head)

  def tail: SortedQueue[A]

  def ++(q: SortedQueue[A]): SortedQueue[A] =
    if (ord == q.ord)
      SortedQueue.merge(this, q)
    else
      throw new RuntimeException("ordering should equal")

  def +(elem: A): SortedQueue[A] = SortedQueue.merge(this, SortedQueue.single(elem))
}

private case class Empty[A]()(implicit override val ord: Ordering[A]) extends SortedQueue[A] {
  val length = 0
  override val rank = 0

  override def head = throw new NoSuchElementException("head on empty priority queue")

  override def tail = throw new NoSuchElementException("tail on empty priority queue")

  override def internalApply(idx: Int): Nothing = head
}

private case class Tree[A](
  length: Int,
  rank: Int,
  head: A,
  left: SortedQueue[A],
  right: SortedQueue[A]
)(implicit override val ord: Ordering[A]) extends SortedQueue[A] {

  override def tail: SortedQueue[A] = SortedQueue.merge(left, right)

  override def internalApply(idx: Int): A =
    if (idx == 0) head
    else tail.internalApply(idx - 1)
}