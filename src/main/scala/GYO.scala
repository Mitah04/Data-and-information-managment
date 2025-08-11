import scala.collection.mutable.ListBuffer

object GyoAlgo {

  var logger: ListBuffer[String] = ListBuffer()
  
  


  def isAcyclic(query: Query): Boolean = {
    logger.clear()
    logger += s"GYO for query: ${query.head}\n"
    var ear = findEar(query)
    while (ear.isDefined) {
      logger += s"Remove ear: ${ear.get}"
      query.body -= ear.get // suppression directe
      logger += s".\n Current query is: ${query}\n"
      ear = findEar(query)
    }
    if (query.body.nonEmpty) logger += "No more ears found"
    writeInFile(c, logger.mkString("\n"))
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
        logger += s" with witness: ${potentialWitnesses.head}"
        Left(potentialWitnesses.head)
      }
      else {
        Right(false)
      }
    }
  }



}