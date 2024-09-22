package grid

import grid.epsilon.{
  EpsilonAllocator,
  GeometricAllocator,
  LinearAllocator,
  UniformAllocator
}
import grid.partitioner.{
  BinaryPartitioner,
  BlockPartitioner,
  EugkmPartitioner,
  MedianPartitioner,
  MultiQuantilePartitioner,
  UniformPartitioner
}
import grid.base.{Block, Point}

class PrivateAbstractor {

  /* Epsilon distribution parameters
   * epsilon = epsilonRelease + epsilonPartition + epsilonThreshold
   * epsilonRelease = epsilon * epsilonReleaseFraction
   * epsilonPartition = epsilon * epsilonPartitionFraction
   * epsilonThreshold = epsilon * epsilonThresholdFraction
   * epsilonReleaseFraction + epsilonPartitionFraction + epsilonThresholdFraction = 1
   * */

  var epsilon: Double = 3.0

  var epsilonReleaseFraction: Double = 1.0 / 3.0
  var epsilonRelease: Double = epsilon * epsilonReleaseFraction

  var epsilonPartitionFraction: Double = 1.0 / 3.0
  var epsilonPartition: Double = epsilon * epsilonPartitionFraction

  var epsilonThresholdFraction: Double = 1.0 / 3.0
  var epsilonThreshold: Double = epsilon * epsilonThresholdFraction

  /* Partitioning parameters
   * partition = "uniform" | "binary" | "median" | "multi-quantile"
   * maxDepth = 10
   * threshold = 10
   * epsilonMode = "exponential" | "uniform"
   * */

  var maxDepth: Int = 10

  var threshold: Double = 10

  var partition: String = "uniform"

  var partitioner: BlockPartitioner = new UniformPartitioner

  var epsilonMode: String = "geometric"

  var allocator: EpsilonAllocator = new GeometricAllocator

  var epsilonPartitioners: List[Double] =
    allocator.createAllocation(epsilonPartition, maxDepth)

  var epsilonThresholds: List[Double] =
    allocator.createAllocation(epsilonThreshold, maxDepth)

  /** Main Setters
    */
  def setEpsilon(newEpsilon: Double): this.type = {
    epsilon = newEpsilon
    setEpsilonPartition(epsilon)
    setEpsilonThreshold(epsilon)
    setEpsilonRelease(epsilon)
    this
  }

  def setPartition(newPartition: String): this.type = {
    partition = newPartition
    setPartitioner(partition match {
      case "binary"         => new BinaryPartitioner
      case "uniform"        => new UniformPartitioner
      case "median"         => new MedianPartitioner
      case "multi-quantile" => new MultiQuantilePartitioner
      case "eugkm"          => new EugkmPartitioner
    })
    this
  }

  def setMaxDepth(newMaxDepth: Int): this.type = {
    maxDepth = newMaxDepth
    setEpsilonPartition(epsilon)
    setEpsilonThreshold(epsilon)
    this
  }

  def setThreshold(newThreshold: Double): this.type = {
    threshold = newThreshold
    this
  }

  def setEpsilonMode(newEpsilonMode: String): this.type = {
    epsilonMode = newEpsilonMode
    setAllocator(epsilonMode match {
      case "uniform"   => new UniformAllocator
      case "linear"    => new LinearAllocator
      case "geometric" => new GeometricAllocator
    })
    this
  }

  /** Dependency Setters
    */

  private def setEpsilonRelease(newEpsilon: Double): this.type = {
    epsilonRelease = newEpsilon * epsilonReleaseFraction
    this
  }
  private def setEpsilonPartition(newEpsilon: Double): this.type = {
    epsilonPartition = newEpsilon * epsilonPartitionFraction
    setEpsilonPartitioners(
      allocator.createAllocation(epsilonPartition, maxDepth)
    )
    this
  }

  private def setEpsilonThreshold(newEpsilon: Double): this.type = {
    epsilonThreshold = newEpsilon * epsilonThresholdFraction
    setEpsilonThresholds(
      allocator.createAllocation(epsilonThreshold, maxDepth)
    )
    this
  }

  private def setAllocator(newAllocator: EpsilonAllocator): this.type = {
    allocator = newAllocator
    setEpsilonPartitioners(
      allocator.createAllocation(epsilonThreshold, maxDepth)
    )
    setEpsilonPartitioners(
      allocator.createAllocation(epsilonPartition, maxDepth)
    )
    this
  }

  private def setPartitioner(newPartitioner: BlockPartitioner): this.type = {
    partitioner = newPartitioner
    this
  }

  private def setEpsilonPartitioners(
      newEpsilonPartitioners: List[Double]
  ): this.type = {
    epsilonPartitioners = newEpsilonPartitioners
    this
  }

  private def setEpsilonThresholds(
      newEpsilonThresholds: List[Double]
  ): this.type = {
    epsilonThresholds = newEpsilonThresholds
    this
  }

  def printParams(): Unit = {
    // prints the params of the private abstractor
    println("epsilon: " + epsilon)
    println("epsilonReleaseFraction: " + epsilonReleaseFraction)
    println("epsilonRelease: " + epsilonRelease)
    println("epsilonPartitionFraction: " + epsilonPartitionFraction)
    println("epsilonPartition: " + epsilonPartition)
    println("epsilonThresholdFraction: " + epsilonThresholdFraction)
    println("epsilonThreshold: " + epsilonThreshold)

    println("maxDepth: " + maxDepth)
    println("threshold: " + threshold)
    println("partition: " + partition)
    println("epsilonMode: " + epsilonMode)
  }
  def privatize(points: List[Point]): List[Point] = {
    val ra = new RecursiveAbstraction
    val initialBlock =
      Block.createInitialBlock(points)

    ra.recursiveAbstractionBudgetSaving(
      initialBlock,
      points,
      threshold,
      partitioner,
      0,
      maxDepth,
      epsilonRelease,
      epsilonThresholds,
      epsilonPartitioners
    )
  }

}
