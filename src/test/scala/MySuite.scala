import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MySuite extends AnyFlatSpec, Matchers:
    "something" should "have the right circumference" in {
      2 must equal (1 + 1);
      "string" must (contain ("st"))
    }