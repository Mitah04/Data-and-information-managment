import TestType.Acyclicity

import scala.collection.mutable.ListBuffer

object AcyclicityTest {

  var logger: ListBuffer[String] = ListBuffer()
  val resultsTxtDir = "results/txt-results/"


  def isAcyclic(query: Query): Int = {
    logger.clear()
    logger += s"GYO for query: ${query.showBody}"
    var ear = findEar(query)
    while (ear.isDefined) {
      query.body -= ear.get // suppression directe
      logger += s"Current query is: ${query.showBody}"
      ear = findEar(query)
    }
    if (query.body.nonEmpty) logger += "No more ears found"
    writeInFile(resultsTxtDir+generateFileName(Acyclicity, query.queryId), logger.mkString("\n"))
    if (query.body.isEmpty) 1 else 0

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
      logger += s"Remove ear: ${atom}"
      Right(true)
    }
    else {
      val potentialWitnesses = otherAtoms.filter { otherAtom =>
        sharedVariables.subsetOf(otherAtom.terms.toSet)
      }

      if (potentialWitnesses.nonEmpty) {
        logger += s"Remove ear: ${atom} with witness: ${potentialWitnesses.head}"
        Left(potentialWitnesses.head)
      }
      else {
        Right(false)
      }
    }
  }



}