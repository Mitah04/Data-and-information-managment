enum TestType {
  case Containment, Acyclicity, Minimality
}

def writeInFile(name:String, content:String): Unit = {
  import java.nio.file.{Files, Paths, StandardOpenOption}
  import java.nio.charset.StandardCharsets

  val path = Paths.get(name)
  Files.write(path, content.getBytes(StandardCharsets.UTF_8),
    StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING,
    StandardOpenOption.WRITE)
  println(s"File written: $name")
}


def generateFileName(testType: TestType, queryId : Int, secondId :Option[Int] = None): String = {
  testType match {
    case TestType.Containment =>
      if (secondId.isDefined) s"test-containment-$queryId-${secondId.get}.txt"
      else
        throw IllegalArgumentException("Second ID must be provided for containment tests.")
    case TestType.Minimality => s"test-minimality-$queryId.txt"
    case TestType.Acyclicity => s"test-acyclicity-$queryId.txt"
  }
}