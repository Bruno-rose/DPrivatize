package grid.base

import scala.math.{max, min}

/** Represents a hyper rectangle or block in a multidimensional space defined by lower and upper bounds.
  *
  * A `Block` object represents a hyper rectangle in a multidimensional space. It is defined by lower and upper bounds
  * for each dimension. The class provides methods for various operations on the block, such as finding its center,
  * adding padding, and generating a human-readable string representation.
  *
  * @param lowerBounds The list of lower bounds for each dimension of the block.
  * @param upperBounds The list of upper bounds for each dimension of the block.
  */
class Block(val lowerBounds: List[Double], val upperBounds: List[Double]) {
  require(
    lowerBounds.length == upperBounds.length,
    "Dimensions of lowerBounds and upperBounds must match"
  )

  /** Alternative constructor that accepts a List of tuples representing (lowerBound, upperBound) pairs for each dimension.
    *
    * @param hyperCube A List of (lowerBound, upperBound) tuples defining the block in each dimension.
    */
  def this(hyperCube: List[(Double, Double)]) {
    this(hyperCube.map(_._1), hyperCube.map(_._2))
  }

  /** Converts the Block's bounds to a List of (Double, Double) tuples for each dimension.
    *
    * @return A List of (lowerBound, upperBound) tuples representing the bounds of the block in each dimension.
    */
  def toListDoubleDouble: List[(Double, Double)] = lowerBounds.zip(upperBounds)

  /** Calculates and returns the center point of the Block.
    *
    * @return A List of Double values representing the center point of the block.
    */
  def center: List[Double] =
    lowerBounds.zip(upperBounds).map { case (minVal, maxVal) =>
      (minVal + maxVal) / 2.0
    }

  /** Creates a new Block by adding padding to the current Block's bounds.
    *
    * @param padding The amount of padding to add to each dimension of the Block.
    * @return A new Block with the added padding.
    */
  def addPadding(padding: Double): Block = {
    val paddedLowerBounds = lowerBounds.map(_ - padding)
    val paddedUpperBounds = upperBounds.map(_ + padding)
    new Block(paddedLowerBounds, paddedUpperBounds)
  }

  /** Returns a human-readable string representation of the Block.
    *
    * The string includes the lower and upper bounds for each dimension of the Block.
    *
    * @return A string representation of the Block.
    */
  override def toString: String =
    s"Block(min:${lowerBounds.mkString(", ")}, max:${upperBounds.mkString(", ")})"
}

/** Companion object for the Block class containing factory methods.
  */
object Block {

  /** Creates an initial Block that bounds a set of points in multidimensional space.
    *
    * @param points The list of points to bound within the Block.
    * @return An initial Block that bounds all the points.
    * @throws IllegalArgumentException if the list of points is empty.
    */
  def createInitialBlock(points: List[Point]): Block = {
    require(points.nonEmpty, "The list of points cannot be empty.")

    // Initialize initial bounds using the first point's vector.
    val initialBounds = (points.head.vector, points.head.vector)

    // Calculate the minimum and maximum bounds that bound all points.
    val (minBounds, maxBounds) = points.foldLeft(initialBounds) {
      case ((minAcc, maxAcc), point) =>
        val updatedMinBounds =
          minAcc.zip(point.vector).map { case (minValue, pointValue) =>
            min(minValue, pointValue)
          }
        val updatedMaxBounds =
          maxAcc.zip(point.vector).map { case (maxValue, pointValue) =>
            max(maxValue, pointValue)
          }
        (updatedMinBounds, updatedMaxBounds)
    }

    // Create and return the initial Block.
    new Block(minBounds, maxBounds)
  }
}
