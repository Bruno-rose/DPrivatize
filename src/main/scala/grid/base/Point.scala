package grid.base


/** Represents a point in a multidimensional space with an associated weight.
  *
  * A `Point` object encapsulates a list of Double values as its vector coordinates in a multidimensional space,
  * along with an optional weight value. It provides methods for converting the point to an OldVector and
  * generating a human-readable string representation.
  *
  * @param vector The list of Double values representing the vector coordinates of the point.
  * @param weight An optional weight associated with the point (default value is 1).
  */
class Point(val vector: List[Double], val weight: Double = 1)
    extends Serializable {

  /** The dimension of the point, which is the length of the vector. */
  val dimension: Int = vector.length

  /** Returns a human-readable string representation of the Point.
    *
    * The string includes the vector coordinates enclosed in square brackets and the associated weight.
    *
    * @return A string representation of the Point.
    */
  override def toString: String =
    s"Point(vector = ${vector.mkString("[", ", ", "]")}, weight = $weight)"
}
