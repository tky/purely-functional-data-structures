package persistent

case class Stack[A](val list: List[A])

object Stack {
  def empty[A]: Stack[A] = new Stack(List.empty)
  def isEmpty[A](stack: Stack[A]): Boolean = stack.list.isEmpty
  def cons[A](h: A, stack: Stack[A]): Stack[A] = new Stack(
    h :: stack.list
  )
  def head[A](stack: Stack[A]): A = stack.list.head
  def tail[A](stack: Stack[A]): Stack[A] = new Stack(stack.list.tail)

  def concat[A](xs: Stack[A], ys: Stack[A]): Stack[A] =
    if (isEmpty(xs)) ys
    else cons(head(xs), concat(tail(xs), ys))
}
