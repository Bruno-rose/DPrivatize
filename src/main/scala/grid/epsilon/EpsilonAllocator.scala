package grid.epsilon

/** A trait that represents an allocator for epsilon values.
  */
trait EpsilonAllocator {

  /** Creates an allocation list based on the provided parameters.
    *
    * @param epsilon the privacy budget
    * @param iterations the number of iterations
    * @return a list of double values representing the allocation
    */
  def createAllocation(epsilon: Double, iterations: Int): List[Double]
}
