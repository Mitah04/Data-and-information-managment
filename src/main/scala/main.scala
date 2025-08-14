//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  //val query1 = parseQuery(1, "Answer(x, y) :- E(x, y), F(y, z).")
  //val query2 = parseQuery(2, "Answer(a, b) :- E(a, b), F(b, c), G(c, d).")
  val q1 = parseQuery(1, "Answer(y, z) :- E(y, z), E(z, z), E(z, w).")
  val q2 = parseQuery(2, "Answer(x, y) :- E(x, y), E(y, z), E(z, y), E(y, w).")
  println(ContainmentTest.isContained(q1, q2))