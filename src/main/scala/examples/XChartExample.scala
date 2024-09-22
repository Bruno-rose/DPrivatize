import org.knowm.xchart._
import org.knowm.xchart.style.Styler
import scala.io.Source
import grid.base.Point
import grid.PrivateAbstractor

object XChartExample {
  def main(args: Array[String]): Unit = {
    val data = readDataFromFile("src/main/scala/examples/data/s1.txt")
    val privatizedData = privatizeData(data)
    
    val chart = createChart()
    addDataToChart(chart, data, privatizedData)
    
    displayAndSaveChart(chart)
  }

  def readDataFromFile(filePath: String): List[Point] = {
    Source.fromFile(filePath).getLines().map { line =>
      val parts = line.trim.split("\\s+")
      new Point(List(parts(0).toDouble, parts(1).toDouble))
    }.toList
  }

  def privatizeData(data: List[Point]): List[Point] = {
    val pointsAbstraction = new PrivateAbstractor
    pointsAbstraction.privatize(points = data)
  }

  def createChart(): BubbleChart = {
    val chart = new BubbleChartBuilder().width(800).height(800)
      .title("Simple Bubble Chart")
      .xAxisTitle("X")
      .yAxisTitle("Y")
      .build()

    chart.getStyler.setLegendPosition(Styler.LegendPosition.InsideNE)
    chart
  }

  def addDataToChart(chart: BubbleChart, originalData: List[Point], privatizedData: List[Point]): Unit = {
    val (dataX, dataY) = originalData.map(p => (p.vector(0), p.vector(1))).unzip
    val dataOnes = originalData.map(_ => 1.0)
    
    val (privatizedDataX, privatizedDataY) = privatizedData.map(p => (p.vector(0), p.vector(1))).unzip
    val privatizedDataW = privatizedData.map(_.weight)

    chart.addSeries("original distribution", dataX.toArray, dataY.toArray, dataOnes.toArray)
    chart.addSeries("privatized distribution", privatizedDataX.toArray, privatizedDataY.toArray, privatizedDataW.toArray)
  }

  def displayAndSaveChart(chart: BubbleChart): Unit = {
    val sw = new SwingWrapper(chart)
    val frame = sw.displayChart()

    println("Press Enter to save the chart and exit...")
    scala.io.StdIn.readLine()

    BitmapEncoder.saveBitmap(chart, "./Sample_Chart", BitmapEncoder.BitmapFormat.PNG)
    println("Chart saved as PNG file.")

    frame.dispose()
  }
}
