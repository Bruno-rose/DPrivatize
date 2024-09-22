package grid.partitioner

import grid.base.{Block, Point}
import scala.util.Random

class MedianPartitioner extends BlockPartitioner {

  /** The axis along which the partitioning is performed.
    */
  var axis: Int = 0

  /** The fanout value for the partitioning.
    */
  private val fanout: Int = 2

  /** Sets the partitioning axis and returns this MedianPartitioner instance.
    *
    * @param value The new axis value.
    * @return This MedianPartitioner instance.
    */
  def setAxis(value: Int): this.type = {
    axis = value
    this
  }

  /** Gets the current partitioning axis.
    *
    * @return The current axis value.
    */
  def getAxis: Int = axis

  /** Samples an item from a list of items based on specified probabilities.
    *
    * @param items         The list of items to sample from.
    * @param probabilities The corresponding probabilities for each item.
    * @return The sampled item.
    * @throws IllegalArgumentException if the input lists are empty or of unequal length.
    */
  def sampleWithProbabilities(
      items: Array[Double],
      probabilities: Array[Double]
  ): Double = {
    if (items.isEmpty || items.length != probabilities.length) {
      throw new IllegalArgumentException(
        "items and probabilities must be non-empty and have equal length"
      )
    } else {
      val rand = Random.nextDouble()
      var cumulativeProbability = 0.0

      for ((item, prob) <- items.zip(probabilities)) {
        cumulativeProbability += prob

        if (rand <= cumulativeProbability) {
          return item
        }
      }
      items.last
    }
  }

  /** Calculates the utility for dynamic programming-based queueing (DPQ) given a list of points and an integer s.
    *
    * @param points The list of points.
    * @param s      An integer value.
    * @return The utility value.
    */
  def utilityDPQ(pointsCount: Double, s: Int): Double = {
    -2 * Math.abs(s - pointsCount / 2.0)
  }

  /** Partitions a block of points using median-based partitioning.
    *
    * @param block   The block to partition.
    * @param points  The list of points within the block.
    * @param epsilon The privacy parameter (epsilon) for differential privacy.
    * @param depth   The depth of the partitioning.
    * @return A list of tuples representing the resulting partitions, each containing a block and a list of points.
    * @throws IllegalArgumentException if the points are empty.
    */
  def partition(
      block: Block,
      points: List[Point],
      epsilon: Double,
      depth: Int
  ): List[(Block, List[Point])] = {
    // TODO: what if the points are empty?
    // TODO: what if the points are all the same?
    this.setAxis(depth % points.head.dimension)

    val pointsLength = points.length

    val axisPoints = points.map(_.vector(axis))

    if (points.isEmpty) {
      throw new IllegalArgumentException(
        "points for median partitioner cannot be empty"
      )
    }

    val pointsWithBounds =
      axisPoints :+ block.lowerBounds(axis) :+ block.upperBounds(axis)

    val sortedPointsWithBounds = pointsWithBounds.toArray.sorted

    val intervals = (0 to pointsLength).map(_.toDouble).toArray

    val density = (0 to pointsLength).toArray.map(i => {
      Math.exp(
        epsilon * utilityDPQ(pointsLength, i)
      ) * (sortedPointsWithBounds(i + 1)
        - sortedPointsWithBounds(i))
    })

    val total = density.sum
    val probabilities = density.map(y => y / total)

    val index = sampleWithProbabilities(intervals, probabilities)

    val divider = Random.nextDouble() * (sortedPointsWithBounds(
      index.toInt + 1
    ) - sortedPointsWithBounds(index.toInt)) + sortedPointsWithBounds(
      index.toInt
    )

    val leftBlock =
      new Block(block.lowerBounds, block.upperBounds.updated(axis, divider))
    val rightBlock =
      new Block(block.lowerBounds.updated(axis, divider), block.upperBounds)

    val leftPoints = points.filter(_.vector(axis) <= divider)
    val rightPoints = points.filter(_.vector(axis) > divider)

    List((leftBlock, leftPoints), (rightBlock, rightPoints))
  }
}
