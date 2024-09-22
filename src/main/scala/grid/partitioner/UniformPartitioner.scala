package grid.partitioner

import grid.base.{Block, Point}
import org.apache.commons.math3.distribution.LaplaceDistribution
import scala.collection.mutable.Map

/** A class representing a uniform partitioner for dividing a space into blocks based on input points.
  *
  * @constructor Create a new UniformPartitioner instance.
  */
class UniformPartitioner extends BlockPartitioner {

  /** Partition the given block into sub-blocks based on the input points and parameters.
    *
    * @param block   The parent block to be partitioned.
    * @param points  The list of points to be partitioned within the block.
    * @param epsilon A parameter for controlling the partition size.
    * @param depth   The depth or level of partitioning.
    * @return A list of sub-blocks with associated points.
    */
  def partition(
      block: Block,
      points: List[Point],
      epsilon: Double,
      depth: Int
  ): List[(Block, List[Point])] = {

    val dim = points.head.dimension
    val M = partitionSize(points, epsilon)
    var axisDivision = axisDivisionAs(dim, M)

    val fanout = Math.pow(axisDivision, dim)
    if (fanout > 1e6) {
      axisDivision = Math.ceil(Math.pow(1e6, 1.0 / dim)).toInt
    }

    if (axisDivision == 1)
      List((block, points))
    else {
      val mapping = cartesianProductMap(axisDivision, dim)

      points.foreach(point => {
        val key = discretizePoint(point, block, axisDivision)
        // println(point, key, mapping(key))
        mapping(key) = point :: mapping(key)
      })

      mapping.map { case (key, points) =>
        (rebuildBlock(block, key, axisDivision), points)
      }.toList
    }
  }

  /** Calculate the partition size based on the given points and epsilon.
    *
    * @param points  The list of points to be partitioned.
    * @param epsilon A parameter for controlling the partition size.
    * @return The calculated partition size.
    */
  def partitionSize(points: List[Point], epsilon: Double): Double = {
    val lapNoisyCount = new LaplaceDistribution(0, 1.0 / epsilon)

    val const: Double = 10
    val dim: Int = points.head.dimension

    val count: Int = points.length
    val noisyCount: Double = Math.max(count + lapNoisyCount.sample(), 0)
    Math.pow(noisyCount * epsilon / const, 2.0 * dim / (2.0 + dim))
  }

  /** Calculate the axis division based on the dimension and M1 value.
    *
    * @param dim The dimension of the space.
    * @param M1  The partition size parameter.
    * @return The calculated axis division.
    */

  def axisDivisionAs(dim: Int, M1: Double): Int = {
    val m1 = Math.pow(M1, 1.0 / dim)
    Math.ceil(Math.max(m1, 1)).toInt
  }

  def discretizePoint(
      point: Point,
      block: Block,
      axisDivision: Int
  ): Seq[Int] = {
    val normalizedCoordinates = point.vector
      .zip(block.lowerBounds.zip(block.upperBounds))
      .map { case (p, (l, u)) => (p - l) / (u - l) }

    normalizedCoordinates.map(coord => (coord * axisDivision).toInt)
  }

  private def cartesianProduct(
      axisDivision: Int,
      dimension: Int
  ): Seq[Seq[Int]] = {
    if (dimension == 1) (0 until axisDivision).map(Seq(_))
    else
      for {
        i <- 0 until axisDivision
        rest <- cartesianProduct(axisDivision, dimension - 1)
      } yield i +: rest
  }
  def cartesianProductMap(
      axisDivision: Int,
      dimension: Int
  ): scala.collection.mutable.Map[Seq[Int], List[Point]] = {
    var cartesianMap = scala.collection.mutable.Map[Seq[Int], List[Point]]()

    cartesianProduct(axisDivision, dimension)
      .foreach(tuple => cartesianMap += (tuple -> List()))

    cartesianMap
  }
  def rebuildBlock(
      block: Block,
      bounds: Seq[Int],
      axisDivision: Int
  ): Block = {
    val minMaxZip = block.lowerBounds.zip(block.upperBounds)

    new Block(minMaxZip.zip(bounds).map { case ((min, max), bound) =>
      val width = (max - min) / axisDivision.toDouble
      val left = min + bound * width
      (left, left + width)
    })
  }

}
