package grid.partitioner

import grid.base.{Block, Point}
import org.apache.commons.math3.distribution.LaplaceDistribution

class EugkmPartitioner extends UniformPartitioner {

  /** Partition the given block into sub-blocks based on the input points and parameters.
    *
    * @param block   The parent block to be partitioned.
    * @param points  The list of points to be partitioned within the block.
    * @param epsilon A parameter for controlling the partition size.
    * @param depth   The depth or level of partitioning.
    * @return A list of sub-blocks with associated points.
    */
  override def partition(
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
      axisDivision = Math.floor(Math.pow(1e6, 1.0 / dim)).toInt
    }

    if (axisDivision == 1)
      List((block, points))
    else {
      val mapping = cartesianProductMap(axisDivision, dim)

      points.foreach(point => {
        val key = discretizePoint(point, block, axisDivision)
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
  override def partitionSize(points: List[Point], epsilon: Double): Double = {
    val lapNoisyCount = new LaplaceDistribution(0, 1.0 / epsilon)

    val const: Double = 10
    val dim: Int = points.head.dimension

    val count: Int = points.length
    val noisyCount: Double = Math.max(count + lapNoisyCount.sample(), 0)
    Math.pow(noisyCount * epsilon / const, 2.0 * dim / (2.0 + dim))
  }

}
