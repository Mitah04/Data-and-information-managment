import scala.collection.mutable.ListBuffer

case class Query(queryId : Int, head : Atom, body : ListBuffer[Atom]) {
  override def toString: String = {
    val bodyStr = if (body.isEmpty) "" else " :- " + body.mkString(", ")
    s"$head$bodyStr."
  }
  
  def showBody: String = {
    if (body.isEmpty) "empty"
    else body.mkString(", ")
  }
}


private def parseQuery(id: Int, rawQuery: String): Query = {
  val Array(headStr, bodyStr) = rawQuery.split(" :- ")
  val head = createAtom(headStr).getOrElse(
    throw new IllegalArgumentException("Invalid head format.")
  )
  val atomPattern = raw"([A-Za-z]+\([^)]*\))".r
  val rawBody = atomPattern.findAllIn(bodyStr)
  val body = rawBody.toList.map(rawAtom =>
    createAtom(rawAtom).getOrElse(
      throw new IllegalArgumentException("Invalid atom format.")
    )
  )
  Query(id, head, ListBuffer.from(body))
}