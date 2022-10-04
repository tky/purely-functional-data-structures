package persistent

package persistent

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CustomStackSpec extends AnyFlatSpec with Matchers {
  "CustomStack" should "return an empty stack" in {
    val stack = CustomStack.empty
    CustomStack.isEmpty(stack) shouldEqual (true)
  }

  "CustomStack" should "concat an item" in {
    val stack = CustomStack.cons(1, CustomStack.empty)
    CustomStack.isEmpty(stack) shouldEqual (false)
  }

  "A CustomStack" should "return a head item" in {
    val stack = CustomStack.cons(1, CustomStack.empty)
    CustomStack.head(stack) shouldEqual (1)
  }

  "A CustomStack" should "return a tail" in {
    val stack = CustomStack.cons(
      3,
      CustomStack.cons(2, CustomStack.cons(1, CustomStack.empty))
    )

    CustomStack.tail(stack) shouldEqual CustomStack.cons(
      2,
      CustomStack.cons(1, CustomStack.empty)
    )
  }

  "Stack" should "concat an empty stack and another stack" in {
    val s1 = CustomStack.empty
    val s2 = CustomStack.cons(3, CustomStack.cons(2, CustomStack.empty))

    CustomStack.concat(s1, s2) shouldEqual CustomStack.cons(
      3,
      CustomStack.cons(2, CustomStack.empty)
    )
  }

  "Stack" should "concat two Stacks" in {
    val s1 = CustomStack.cons(1, CustomStack.empty)
    val s2 = CustomStack.cons(3, CustomStack.cons(2, CustomStack.empty))

    CustomStack.concat(s2, s1) shouldEqual CustomStack.cons(
      3,
      CustomStack.cons(2, CustomStack.cons(1, CustomStack.empty))
    )
  }

  "A Stack" should "update" in {
    val stack = CustomStack.cons(
      3,
      CustomStack.cons(2, CustomStack.cons(1, CustomStack.empty))
    )
    val updated = CustomStack.update(stack, 1, 8)

    updated shouldEqual CustomStack.cons(
      3,
      CustomStack.cons(8, CustomStack.cons(1, CustomStack.empty))
    )
  }
}
