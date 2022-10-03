package persistent

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StackSpec extends AnyFlatSpec with Matchers {
  "Stack" should "return an empty stack" in {
    val stack = Stack.empty
    Stack.isEmpty(stack) shouldEqual (true)
  }

  "Stack" should "concat an item" in {
    val stack = Stack.empty[Int]
    val next = Stack.cons(1, stack)
    Stack.isEmpty(next) shouldEqual (false)

    Stack.isEmpty(stack) shouldEqual (true)
  }

  "A Stack" should "return an head item" in {
    val stack = Stack.empty[Int]
    val next = Stack.cons(1, stack)

    Stack.head(next) shouldEqual (1)
  }

  "A Stack" should "return a tail" in {
    val stack = Stack.cons(3, Stack.cons(2, Stack.cons(1, Stack.empty)))

    Stack.tail(stack) shouldEqual (Stack.cons(2, Stack.cons(1, Stack.empty)))
  }

  "A Stack" should "concat another Stack" in {
    val stack1 = Stack.cons(3, Stack.cons(2, Stack.cons(1, Stack.empty)))
    val stack2 = Stack.cons(5, Stack.cons(4, Stack.empty))

    Stack.concat(stack2, stack1) shouldEqual (Stack(List(5, 4, 3, 2, 1)))
  }
}
