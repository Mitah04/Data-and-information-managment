object GyoAlgo {

  def isAcyclic(query: Query) = {

  }

  def isAnEar(query: Query, atom: Atom) : Either[Atom, Boolean] = {
    val nonSharedEdges: List[String] =
      atom.terms.filter(v => isContained(query, List(v), atom).isEmpty)
    if (nonSharedEdges.length == atom.terms.length)
      Right(true) 
    else {
      val witness = isWitness(query, atom.terms.diff(nonSharedEdges), atom)
      if witness.isEmpty
        Right(false)
      else
        Left(witness[0])
        
      }

  }

  def isContained(query: Query, terms: List[String], current: Atom): List[Atom] =
    query.body.filter(e => e != current && terms.exists(e.terms.contains))

  def isWitness(query: Query, terms: List[String], current: Atom): List[Atom] =
    query.body.filter(e => e != current && terms.forall(e.terms.contains))


}