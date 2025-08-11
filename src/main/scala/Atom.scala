case class Atom(relationName: String, terms: List[String]) {
  override def toString: String = {
    val termsStr = if (terms.isEmpty) "" else "(" + terms.mkString(", ") + ")"
    s"$relationName$termsStr"
  }

  def arity: Int = terms.length

  def isGround: Boolean = terms.forall(_.forall(_.isLetterOrDigit))
}

def createAtom(rawAtom: String): Option[Atom] = {
  val namePattern = raw"([a-zA-Z]*)\(.*\)".r
  val termPattern = raw"[A-Za-z]+\(([^)]*)\)".r

  val nameOpt = namePattern.findFirstMatchIn(rawAtom).map(_.group(1))
  val insideOpt = termPattern.findFirstMatchIn(rawAtom).map(_.group(1)) 

  (nameOpt, insideOpt) match {
    case (Some(name), Some(inside)) =>
      val args = raw"[^,\s)]+".r.findAllIn(inside).toList
      Some(Atom(name, args))
    case _ => None
  }
}