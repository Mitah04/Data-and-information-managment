case class Query(queryId : Int, head : Atom, body : List[Atom]) {
  override def toString: String = {
    val bodyStr = if (body.isEmpty) "" else " :- " + body.mkString(", ")
    s"$head$bodyStr."
  }
}


def parseQuery(id: Int, rawQuery: String): Query = {
  val Array(headStr, bodyStr) = rawQuery.split(" :- ")
  val head = createAtom(headStr).getOrElse(
    throw new IllegalArgumentException("Invalid head format.")
  )
  val rawBody = bodyStr.split(",")
  val body = rawBody.toList.map(rawAtom =>
    createAtom(rawAtom).getOrElse(
      throw new IllegalArgumentException("Invalid atom format.")
    )
  )
  Query(id, head, body)
}

