//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  val query1 = parseQuery(6, "Answer() :- A(x, y), B(y, z), C(z, w, u, v).")
  val Some(test) = createAtom("A(x, y)")
  println(GyoAlgo.isAnEar(query1, test))