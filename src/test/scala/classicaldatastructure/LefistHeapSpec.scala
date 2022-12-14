package classicaldatastructure

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LefistHeapSpec extends AnyFlatSpec with Matchers {

  "LefistHeap#empty" should "return an empty Heap" in {
    val heap = LefistHeap.empty
    LefistHeap.isEmpty(heap) shouldEqual (true)
  }

  "LefistHeap#insert" should "insert a value to a Heap" in {
    val heap = LefistHeap.empty
    val h2 = LefistHeap.insert("A", heap)

    LefistHeap.isEmpty(h2) shouldEqual (false)
    LefistHeap.findMin(h2) shouldEqual ("A")

    val h3 = LefistHeap.insert("B", h2)
    LefistHeap.findMin(h2) shouldEqual ("A")

    val h4 = LefistHeap.deleteMin(h3)
    LefistHeap.findMin(h4) shouldEqual ("B")
  }

  "LefistHeap#insert2" should "insert a value to a Heap" in {
    val heap = LefistHeap.empty
    val h2 = LefistHeap.insert2("A", heap)

    LefistHeap.isEmpty(h2) shouldEqual (false)
    LefistHeap.findMin(h2) shouldEqual ("A")

    val h3 = LefistHeap.insert2("B", h2)
    LefistHeap.findMin(h2) shouldEqual ("A")
  }

  "LefistHeap#fromList" should "create a Heap from a List" in {
    val heap = LefistHeap.fromList(List("C", "A", "B", "D"))

    LefistHeap.findMin(heap) shouldEqual ("A")
    val h2 = LefistHeap.deleteMin(heap)
    LefistHeap.findMin(h2) shouldEqual ("B")
    val h3 = LefistHeap.deleteMin(h2)
    LefistHeap.findMin(h3) shouldEqual ("C")
    val h4 = LefistHeap.deleteMin(h3)
    LefistHeap.findMin(h4) shouldEqual ("D")
    val h5 = LefistHeap.deleteMin(h4)
    LefistHeap.isEmpty(h5) shouldEqual (true)
  }
}
