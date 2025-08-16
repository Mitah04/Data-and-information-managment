import java.io.File
import scala.io.Source
import scala.collection.mutable.ListBuffer

object Main extends App {

  println("Starting CQ Engine Project...")

  // --- 1. Setup File Paths ---
  val dataDir = "data/"
  val resultsCsvDir = "results/csv-results/"
  val resultsTxtDir = "results/txt-results/"
  // Ensure results directories exist
  new File(resultsCsvDir).mkdirs()
  new File(resultsTxtDir).mkdirs()


  // --- 2. Load All Queries from the data/ Directory ---
  val queries = loadQueriesFromDirectory(dataDir)
  println(s"Loaded ${queries.size} queries from '$dataDir'.")


  // --- 3. Run Acyclicity and Minimality Tests ---
  println("\n--- Running Acyclicity and Minimality tests ---")
  val mainOutputRows = queries.map { query =>
    println(s"Testing Query ${query.queryId}...")

    // Run Acyclicity Test
    val queryForAcyclicity = query.copy(body = ListBuffer.from(query.body))
    val isAcyclicResult = AcyclicityTest.isAcyclic(queryForAcyclicity)

    // Run Minimality Test
    // Create a deep copy of the query for the minimality test so it doesn't get modified
    val queryForMinimality = query.copy(body = ListBuffer.from(query.body))
    val isMinimalResult = MinimalityTest.isMinimal(queryForMinimality)

    s"${query.queryId},$isAcyclicResult,$isMinimalResult"
  }

  // Write main-output.csv
  val mainCsvContent = "queryId,isAcyclic,isMinimal\n" + mainOutputRows.mkString("\n")
  writeInFile(resultsCsvDir + "main-output.csv", mainCsvContent)


  // --- 4. Run Containment Tests for all pairs ---
  println("\n--- Running Containment tests ---")
  val containmentOutputRows = for {
    q1 <- queries
    q2 <- queries
  } yield {
    println(s"Testing if q${q1.queryId} is contained in q${q2.queryId}...")
    // Important: create deep copies to avoid modifications between tests
    val q1_copy = q1.copy(body = ListBuffer.from(q1.body))
    val q2_copy = q2.copy(body = ListBuffer.from(q2.body))
    val isContainedResult = ContainmentTest.isContainedIn(q1_copy, q2_copy)
    s"${q1.queryId},${q2.queryId},$isContainedResult"
  }

  // Write containment-output.csv
  val containmentCsvContent = "queryId1,queryId2,isContainedIn\n" + containmentOutputRows.mkString("\n")
  writeInFile(resultsCsvDir + "containment-output.csv", containmentCsvContent)

  println("\nProject execution finished successfully!")


  /**
   * Helper function to load all query-*.txt files from a directory.
   */
  def loadQueriesFromDirectory(dirPath: String): List[Query] = {
    val dir = new File(dirPath)
    if (dir.exists && dir.isDirectory) {
      dir.listFiles
        .filter(_.isFile)
        .filter(_.getName.startsWith("query-"))
        .toList
        .flatMap { file =>
          val queryId = file.getName.stripPrefix("query-").stripSuffix(".txt").toInt
          val content = Source.fromFile(file).getLines().mkString
          try {
            Some(parseQuery(queryId, content))
          } catch {
            case e: Exception =>
              println(s"Error parsing file ${file.getName}: ${e.getMessage}")
              None
          }
        }
    } else {
      println(s"Warning: Directory '$dirPath' not found.")
      List.empty
    }
  }
}