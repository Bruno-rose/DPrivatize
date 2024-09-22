package grid

import grid.partitioner.EugkmPartitioner

import grid.base.{Block, Point}
import org.apache.commons.math3.distribution.LaplaceDistribution

class EugmkAbstractor extends PrivateAbstractor {
  override def setEpsilon(newEpsilon: Double): this.type = {
    epsilon = newEpsilon
    this
  }
  override def privatize(points: List[Point]): List[Point] = {
    val partitioner = new EugkmPartitioner
    val initialBlock =
      Block.createInitialBlock(points)
    val block = initialBlock.addPadding(1e-5)

    val partition: List[(Block, List[Point])] =
      partitioner.partition(
        block,
        points,
        epsilon,
        0
      )

    val laplaceRelease = new LaplaceDistribution(0, 1.0 / epsilon)

    partition.flatMap { case (block, points) =>
      List(
        new Point(
          block.center,
          math.max(points.map(_.weight).sum + laplaceRelease.sample(), 0)
        )
      )
    }
  }
}
