object GyoAlgo {

  def isAcyclic(query: Query) = {

  }

  /**
   * Checks if a given atom is an "ear" in the context of a query.
   *
   * @param query The full query.
   * @param atom  The atom to check.
   * @return Either a Left(witness: Atom) if it's an ear with a witness,
   *         or a Right(isEar: Boolean) where true means it's an "island" ear
   *         and false means it's not an ear at all.
   */
  def isAnEar(query: Query, atom: Atom): Either[Atom, Boolean] = {
    val atomVariables = atom.terms.toSet
    val otherAtoms = query.body.filter(_ != atom)

    val sharedVariables = atomVariables.filter { variable =>
      otherAtoms.exists(_.terms.contains(variable))
    }

    if (sharedVariables.isEmpty) {
      Right(true)
    }
    else {
      val potentialWitnesses = otherAtoms.filter { otherAtom =>
        sharedVariables.subsetOf(otherAtom.terms.toSet)
      }

      if (potentialWitnesses.nonEmpty) {
        Left(potentialWitnesses.head)
      }
      else {
        Right(false)
      }
    }
  }

  def isContained(query: Query, terms: List[String], current: Atom): List[Atom] =
    query.body.filter(e => e != current && terms.exists(e.terms.contains))

  def isWitness(query: Query, terms: List[String], current: Atom): List[Atom] =
    query.body.filter(e => e != current && terms.forall(e.terms.contains))


}