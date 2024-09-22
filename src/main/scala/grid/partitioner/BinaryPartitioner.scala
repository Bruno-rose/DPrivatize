/** Package: grid.partitioner
  *
  * This package contains classes related to partitioning blocks and points in a grid.
  */
package grid.partitioner

import grid.base.{Block, Point}

/** BinaryPartitioner is a class that implements block partitioning using a binary splitting strategy.
  * It divides a block into two smaller blocks based on the specified axis.
  *
  * @note For each level of depth, the axis of division is determined by the remainder of the depth
  *       divided by the dimension of the points.
  */
class BinaryPartitioner extends BlockPartitioner {
  //TODO: for each level, we need to divide by four not two
  private var axis: Int = 0

  /** The fanout value for the partitioning.
    */
  private val fanout: Int = 2

  /** Set the axis of division for the BinaryPartitioner.
    *
    * @param value The axis index along which the block should be divided.
    * @return This BinaryPartitioner instance to allow method chaining.
    */
  def setAxis(value: Int): this.type = {
    axis = value
    this
  }

  /** Get the currently configured axis of division.
    *
    * @return The axis index along which the block will be divided.
    */
  def getAxis: Int = axis

  /** Partition a block into two smaller blocks and distribute points accordingly.
    *
    * @param block   The block to be partitioned.
    * @param points  The list of points to be distributed into the sub-blocks.
    * @param epsilon A value for controlling the partitioning process (unused in this implementation).
    * @param depth   The current depth level in the partitioning process.
    * @return A list of pairs, each containing a smaller block and the list of points assigned to it.
    */
  def partition(
      block: Block,
      points: List[Point],
      epsilon: Double,
      depth: Int
  ): List[(Block, List[Point])] = {
    // Set the axis of division based on the depth level.
    this.setAxis(depth % points.head.dimension)

    // Calculate the dividing point along the chosen axis.
    val divider = (block.lowerBounds(axis) + block.upperBounds(axis)) / 2

    // Create two new blocks by dividing along the chosen axis.
    val leftBlock =
      new Block(block.lowerBounds, block.upperBounds.updated(axis, divider))
    val rightBlock =
      new Block(block.lowerBounds.updated(axis, divider), block.upperBounds)

    // Divide points into two groups based on the dividing point.
    val leftPoints = points.filter(_.vector(axis) <= divider)
    val rightPoints = points.filter(_.vector(axis) > divider)

    // Return a list of pairs, each containing a smaller block and the list of points assigned to it.
    List((leftBlock, leftPoints), (rightBlock, rightPoints))
  }
}
