package grid.partitioner

import grid.base.{Block, Point}
import dp_multiq.JointExp.jointExp

/** QuantilePartitioner is a class that implements a partitioning strategy for points within a Block
  * based on quantile-based division along a specified axis. This class extends the BlockPartitioner
  * abstract class.
  *
  * @constructor Create a new QuantilePartitioner instance.
  */
class MultiQuantilePartitioner extends BlockPartitioner {

  // Private member variable to store the axis used for partitioning
  private var axis: Int = 0

  /** The fanout value for the partitioning.
    */
  private val fanout: Int = 4

  /** Set the axis to be used for partitioning.
    *
    * @param value The index of the axis to use for partitioning.
    * @return This QuantilePartitioner instance to allow method chaining.
    */
  def setAxis(value: Int): this.type = {
    axis = value
    this
  }

  /** Get the currently set partitioning axis.
    *
    * @return The index of the axis used for partitioning.
    */
  def getAxis: Int = axis

  /** Partition a Block into sub-blocks based on quantile-based division along the specified axis.
    *
    * @param block   The Block to be partitioned.
    * @param points  A list of Point objects to be partitioned within the Block.
    * @param epsilon The epsilon parameter for the jointExp function used in partitioning.
    * @param depth   The current depth of the partitioning operation.
    * @return A list of pairs, where each pair consists of a sub-block and a list of Points within that sub-block.
    * @throws Exception if a sanity check fails, indicating missing or duplicated points.
    */
  def partition(
      block: Block,
      points: List[Point],
      epsilon: Double,
      depth: Int
  ): List[(Block, List[Point])] = {

    // Set the partitioning axis based on the depth
    this.setAxis(depth % points.head.dimension)

    // TODO: optimizar sort, usar unicamente array
    val dividers = jointExp(
      points.map(_.vector(axis)).toArray.sorted,
      block.lowerBounds(axis),
      block.upperBounds(axis),
      (1 until fanout).map(_.toDouble).toArray.map(_ / fanout.toDouble),
      epsilon,
      swap = true
    )
    val intervals =
      block.lowerBounds(axis) +: dividers :+ block.upperBounds(axis)

    val result =
      (0 until intervals.length - 1)
        .map(i =>
          new Block(
            block.lowerBounds.updated(axis, intervals(i)),
            block.upperBounds.updated(axis, intervals(i + 1))
          )
        )
        .toList
        .zipWithIndex
        .map { case (b, i) =>
          (
            b,
            points.filter(p =>
              p.vector(axis) >= intervals(i) && p.vector(axis) < intervals(
                i + 1
              )
            )
          )
        }

    result
  }

}
