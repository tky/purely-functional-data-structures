package persistent.tree

sealed abstract class Tree[+A]

case class Branch[A](
    value: A,
    left: Tree[A],
    right: Tree[A]
) extends Tree[A]

case object Empty extends Tree

object Tree {
  def member[A](x: A, t: Tree[A])(implicit ordering: Ordering[A]): Boolean =
    t match {
      case Empty                     => false
      case Branch(v, _, _) if v == x => true
      case Branch(v, left, right) =>
        if (ordering.compare(v, x) > 0) member(x, left) else member(x, right)
    }

  def insert[A](x: A, t: Tree[A])(implicit ordering: Ordering[A]): Tree[A] =
    t match {
      case Empty => Branch(x, Empty, Empty)
      case Branch(v, l, r) =>
        if (ordering.compare(x, v) < 0) Branch(v, insert(x, l), r)
        else if (ordering.compare(x, v) > 0) Branch(v, l, insert(x, r))
        else t
    }

  // ex2.2
  def member2[A](x: A, t: Tree[A])(implicit ordering: Ordering[A]): Boolean = {
    def _go(x: A, t: Tree[A], tmp: A): Boolean = t match {
      case Empty                   => false
      case Branch(v, Empty, Empty) => v == x || tmp == x
      case Branch(v, left, right) =>
        if (ordering.compare(v, x) > 0) _go(x, left, v) else _go(x, right, v)
    }

    t match {
      case Empty           => false
      case Branch(v, _, _) => _go(x, t, v)
    }
  }

  // ex2.3
  def insert2[A](x: A, t: Tree[A])(implicit ordering: Ordering[A]): Tree[A] =
    t match {
      case Empty => Branch(x, Empty, Empty)
      case Branch(v, l, r) =>
        if (ordering.compare(x, v) < 0) Branch(v, insert(x, l), r)
        else if (ordering.compare(x, v) > 0) Branch(v, l, insert(x, r))
        else throw new IllegalArgumentException()
    }

  // ex2.4
  // ???????
  def insert3[A](x: A, t: Tree[A])(implicit ordering: Ordering[A]): Tree[A] =
    t match {
      case Empty => Branch(x, Empty, Empty)
      case Branch(v, l, r) =>
        if (ordering.compare(x, v) < 0) Branch(v, insert(x, l), r)
        else if (ordering.compare(x, v) > 0) Branch(v, l, insert(x, r))
        else throw new IllegalArgumentException()
    }

  def complete[A](x: A, d: Int): Tree[A] = d match {
    case 0 => Branch(x, Empty, Empty)
    case _ => {
      val p = complete(x, d - 1)
      Branch(x, p, p)
    }
  }
}
