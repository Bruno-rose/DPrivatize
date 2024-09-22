package grid.epsilon

/** A class that implements uniform allocation by evenly distributing the privacy budget across iterations.
  */
class UniformAllocator extends EpsilonAllocator {

  /** Creates an allocation list based on the uniform allocation algorithm.
    *
    * @param epsilon the privacy budget
    * @param iterations the number of iterations
    * @return a list of double values representing the uniform allocation
    */
  def createAllocation(epsilon: Double, iterations: Int): List[Double] = {
    (0 until iterations).map(_ => epsilon / iterations).toList
  }
}
