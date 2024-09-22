package grid.partitioner

import grid.base.{Block, Point}

trait BlockPartitioner {
  def partition(
      block: Block,
      points: List[Point],
      epsilon: Double,
      depth: Int
  ): List[(Block, List[Point])]
}
