import ContainmentTest.isContained
import org.scalatest.funsuite.AnyFunSuite

class ContainmentSpec extends AnyFunSuite {
  test("containment") {
    val q1 = parseQuery(1, "Answer(y, z) :- E(y, z), E(z, z), E(z, w).")
    val q2 = parseQuery(2, "Answer(x, y) :- E(x, y), E(y, z), E(z, y), E(y, w).")
    assert(isContained(q1, q2))
  }

  test("non containment") {
    val q3 = parseQuery(3, "Answer(x) :- E(x, y), F(y, z).")
    val q4 = parseQuery(4, "Answer(a) :- E(a, b), G(b, c).")
    assert(!isContained(q3, q4))
  }
}
