import scala.io.Source
import grid.PrivateAbstractor
import grid.base.Point

object Main {
  def main(args: Array[String]): Unit = {
    // Read and parse data from file
    val inputFile = "src/main/scala/examples/data/s1.txt"
    val data = readPointsFromFile(inputFile)

    // Privatize the data
    val privatizer = new PrivateAbstractor
    val privatizedData = privatizer.privatize(data)

    // Print the privatized data
    printPoints(privatizedData)
  }

  def readPointsFromFile(filePath: String): List[Point] = {
    Source.fromFile(filePath).getLines().map { line =>
      val coordinates = line.trim.split("\\s+").map(_.toDouble)
      new Point(List(coordinates(0), coordinates(1)))
    }.toList
  }

  def printPoints(points: List[Point]): Unit = {
    points.foreach(println)
  }
}