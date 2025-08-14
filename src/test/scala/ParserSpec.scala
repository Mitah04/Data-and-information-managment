import org.scalatest.funsuite.AnyFunSuite

class ParserSpec extends AnyFunSuite {
  test("Parse a simple query") {
    val q = parseQuery(1, "Answer(x, y) :- E(x, y), F(y, z), G(z).")
    assert(q.queryId == 1)
    assert(q.head.relationName == "Answer")
    assert(q.head.terms == List("x","y"))
    assert(q.body.size == 3)
    assert(q.body.exists(a => a.relationName=="E" && a.terms==List("x","y")))
  }
}
