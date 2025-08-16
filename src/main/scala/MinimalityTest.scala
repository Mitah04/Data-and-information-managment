
import scala.collection.mutable.ListBuffer

object MinimalityTest {

  val logger = ListBuffer[String]()
  val resultsTxtDir = "results/txt-results/"


  def isMinimal(originalQuery: Query): Int = {
    logger.clear()
    logger += s"Minimization for query: ${originalQuery}"

    val minimizedBody = ListBuffer.from(originalQuery.body)
    var wasModified = false

    var atomWasRemovedInPass = true
    while (atomWasRemovedInPass) {
      atomWasRemovedInPass = false
      var foundAndRemoved: Option[Atom] = None

      val it = minimizedBody.iterator
      while (it.hasNext && !atomWasRemovedInPass) {
        val atom = it.next()

        val smallerBody = minimizedBody.filter(_ != atom)
        val q_smaller = originalQuery.copy(body = smallerBody)

        val homomorphismOpt = ContainmentTest.check(q_smaller, originalQuery)

        if (homomorphismOpt.isDefined) {
          foundAndRemoved = Some(atom)
          atomWasRemovedInPass = true
          wasModified = true

          logger += s"Remove atom ${atom} by the virtue of the homomorphism containing the following mappings:"
          homomorphismOpt.get.foreach { case (from, to) =>
            logger += s"${from} to ${to}"
          }
        }
      }

      if (atomWasRemovedInPass) {
        minimizedBody -= foundAndRemoved.get
        logger += s"Current query is: ${originalQuery.copy(body = minimizedBody)}"
      }
    }

    logger += "No more atoms can be removed."

    val finalQuery = originalQuery.copy(body = minimizedBody)
    val isMinimal = !wasModified

    if (finalQuery.body != originalQuery.body)
      writeInFile(resultsTxtDir+ generateFileName(TestType.Minimality, originalQuery.queryId), logger.mkString("\n"))

    if (isMinimal) 1 else 0
  }
}