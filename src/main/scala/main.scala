//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  val query1 = parseQuery(6, "Answer() :- A(x, y,z), B(y, t), C(z, w, u, v).")
  val Some(test) = createAtom("A(x,y,z)")
  println(GyoAlgo.isContained(query = query1, atom = test))
