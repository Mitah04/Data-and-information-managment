import scala.collection.mutable.ListBuffer

object GyoAlgo {


  def isAcyclic(query: Query): Boolean = {
    var ear = findEar(query)
    while (ear.isDefined) {
      println(ear.get)
      query.body -= ear.get // suppression directe
      ear = findEar(query)
    }
    query.body.isEmpty
  }


  def findEar(query: Query): Option[Atom] =
    query.body.find { a =>
      isAnEar(query, a) match {
        case Left(_) => true
        case Right(true) => true
        case _ => false
      }
    }


  private def isAnEar(query: Query, atom: Atom): Either[Atom, Boolean] = {
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



}