package grid

import grid.partitioner.BlockPartitioner
import grid.base.{Block, Point}
import org.apache.commons.math3.distribution.LaplaceDistribution

class RecursiveAbstraction {
  def recursiveAbstractionBudgetSaving(
      block: Block,
      points: List[Point],
      threshold: Double,
      partitioner: BlockPartitioner,
      depth: Int,
      maxDepth: Int,
      epsilonRelease: Double,
      epsilonThresholds: List[Double],
      epsilonPartitioners: List[Double],
      verbose: Boolean = false
  ): List[Point] = {
    if (depth >= maxDepth || points.isEmpty) {
      // TODO: is points.isEmpty differentially private?
      // TODO: Should we add extra epsilon in the case the points are empty?
      val laplaceRelease = new LaplaceDistribution(0, 1.0 / epsilonRelease)

      // TODO: Try with negative points
      if (verbose)
        println(
          "max depth or empty - stopped at depth: " +
            depth + " with points: " + points
              .map(_.weight)
              .sum + " and threshold: " + threshold
        )

      List(
        new Point(
          block.center,
          math.max(points.map(_.weight).sum + laplaceRelease.sample(), 0)
        )
      )
    } else {

      val laplaceThreshold =
        new LaplaceDistribution(0, 1.0 / epsilonThresholds(depth))

      val count: Double = points.map(_.weight).sum
      val noisyCount = math.max(count + laplaceThreshold.sample(), 0)

      // Stopping condition for recursion
      if (noisyCount <= threshold) {
        if (verbose)
          println(
            "threshold - stopped at depth         : " +
              depth + " with points: " + points
                .map(_.weight)
                .sum + " and threshold: " + threshold
          )

        val epsilonsExtraRelease = epsilonThresholds
          .takeRight(depth + 1)
          .sum + epsilonPartitioners.takeRight(depth + 1).sum

        val laplaceExtraRelease = new LaplaceDistribution(
          0,
          1.0 / (epsilonRelease + epsilonsExtraRelease)
        )

        List(
          new Point(
            block.center,
            math.max(points.map(_.weight).sum + laplaceExtraRelease.sample(), 0)
          )
        )

      } else {

        val paddedBlock = block.addPadding(1e-6)

        val partition: List[(Block, List[Point])] =
          partitioner.partition(
            paddedBlock,
            points,
            epsilonPartitioners(depth),
            depth
          )

        partition.flatMap { case (subBlock, subPoints) =>
          recursiveAbstractionBudgetSaving(
            subBlock,
            subPoints,
            threshold,
            partitioner,
            depth + 1,
            maxDepth,
            epsilonRelease,
            epsilonThresholds,
            epsilonPartitioners
          )
        }
      }
    }
  }
}
