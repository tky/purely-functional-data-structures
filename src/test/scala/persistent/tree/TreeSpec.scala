package persistent.tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {
  "Tree" should "return true if contains the target" in {
    val tree = Branch(1, Empty, Empty)
    Tree.member(1, tree) shouldEqual true

    val tree2 = Branch(2, Branch(1, Empty, Empty), Branch(3, Empty, Empty))
    Tree.member(2, tree2) shouldEqual true
    Tree.member(3, tree2) shouldEqual true
  }

  "Tree" should "return false if does not contain the target" in {
    val tree2 = Branch(2, Branch(1, Empty, Empty), Branch(3, Empty, Empty))
    Tree.member(4, tree2) shouldEqual false
  }

  "Tree" should "insert a value to an empty tree" in {
    Tree.insert(1, Empty) shouldEqual Branch(1, Empty, Empty)
  }

  "Tree" should "insert a value to the left side" in {
    val tree = Branch(2, Empty, Empty)
    Tree.insert(1, tree) shouldEqual Branch(2, Branch(1, Empty, Empty), Empty)
  }

  "Tree" should "insert a value to the right side" in {
    val tree = Branch(2, Empty, Empty)
    Tree.insert(3, tree) shouldEqual Branch(2, Empty, Branch(3, Empty, Empty))
  }
}
