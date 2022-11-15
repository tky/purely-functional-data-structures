package classicaldatastructure

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LefistHeapSpec extends AnyFlatSpec with Matchers {

  "LefistHeap#empty" should "return an empty Heap" in {
    val heap = LefistHeap.empty
    LefistHeap.isEmpty(heap) shouldEqual (true)
  }

  "LefistHeap#insert" should "insert a value to the empty Heap" in {
    val heap = LefistHeap.empty
    val h2 = LefistHeap.insert("A", heap)

    LefistHeap.isEmpty(h2) shouldEqual (false)
    LefistHeap.findMin(h2) shouldEqual ("A")

    val h3 = LefistHeap.insert("B", h2)
    LefistHeap.findMin(h2) shouldEqual ("A")

    val h4 = LefistHeap.deleteMin(h3)
    LefistHeap.findMin(h4) shouldEqual ("B")
  }
}
