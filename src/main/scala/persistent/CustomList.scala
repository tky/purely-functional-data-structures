package persistent

trait CustomStack[+A]

case class CustomList[A](h: A, tail: CustomStack[A]) extends CustomStack[A]
case object Empty extends CustomStack[Nothing]

object CustomStack {
  def empty[A]: CustomStack[A] = Empty
  def isEmpty[A](stack: CustomStack[A]): Boolean = stack match {
    case Empty => true
    case _     => false
  }

  def cons[A](h: A, stack: CustomStack[A]): CustomStack[A] =
    new CustomList(h, stack)

  def head[A](stack: CustomStack[A]): A = stack match {
    case CustomList(h, _) => h
    case _                => throw (new NoSuchElementException)
  }

  def tail[A](stack: CustomStack[A]): CustomStack[A] = stack match {
    case CustomList(_, tail) => tail
    case _                   => throw (new NoSuchElementException)
  }

  def concat[A](xs: CustomStack[A], ys: CustomStack[A]): CustomStack[A] =
    xs match {
      case Empty               => ys
      case CustomList(h, tail) => cons(h, concat(tail, ys))
    }

  def update[A](xs: CustomStack[A], i: Int, y: A): CustomStack[A] =
    (i, xs) match {
      case (0, CustomList(h, hs)) => cons(y, hs)
      case (_, Empty)             => cons(y, empty)
      case (_, CustomList(h, hs)) => cons(h, update(hs, i - 1, y))
    }
}
