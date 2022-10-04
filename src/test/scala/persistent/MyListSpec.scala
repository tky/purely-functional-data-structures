package persistent

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyListSpec extends AnyFlatSpec with Matchers {
  "suffixes" should "return list" in {
    MyList.suffixes(List(1, 2, 3, 4)) shouldEqual List(
      List(1, 2, 3, 4),
      List(2, 3, 4),
      List(3, 4),
      List(4),
      List()
    )
  }
}
