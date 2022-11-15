package classicaldatastructure

sealed abstract class Heap[+A]
case object E extends Heap
case class T[A](r: Int, e: A, left: Heap[A], right: Heap[A]) extends Heap[A]

object LefistHeap {

  def empty = E

  def isEmpty[A](h: Heap[A]): Boolean = h match {
    case E => true
    case _ => false
  }

  def merge[A](h1: Heap[A], h2: Heap[A])(implicit
      ordering: Ordering[A]
  ): Heap[A] = (h1, h2) match {
    case (E, _) => h2
    case (_, E) => h1
    case (T(_, x, a1, b1), T(_, y, a2, b2)) =>
      if (ordering.compare(x, y) < 0) makeT(x, a1, merge(b1, h2))
      else makeT(y, a2, merge(h1, b2))
  }

  def insert[A](x: A, h: Heap[A])(implicit ordering: Ordering[A]): Heap[A] =
    merge(T(1, x, E, E), h)

  def findMin[A](h: Heap[A]): A = h match {
    case E => throw new IllegalStateException("Empty Heap is passed.")
    case T(_, x, _, _) => x
  }

  def deleteMin[A](h: Heap[A])(implicit ordering: Ordering[A]) = h match {
    case E => throw new IllegalStateException("Empty Heap is passed.")
    case T(_, x, a, b) => merge(a, b)
  }

  private def rank[A](h: Heap[A]): Int = h match {
    case E             => 0
    case T(r, _, _, _) => r
  }

  private def makeT[A](x: A, a: Heap[A], b: Heap[A]): Heap[A] =
    if (rank(a) >= rank(b)) T(rank(b) + 1, x, a, b) else T(rank(a), x, b, a)
}
