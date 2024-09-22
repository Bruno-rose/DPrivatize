package grid.epsilon

/** A class that implements geometric allocation by allocating an increasing fraction of the privacy budget at each iteration.
  */
class GeometricAllocator extends EpsilonAllocator {

  /** To allocate `epsilon` privacy budget among `iterations` iterations,
    * with a increasing fraction of `fraction` at each iteration, we use the formula:
    *
    * `epsilon` = `alpha` * `ratio`^0^  + `alpha` * `ratio`^1^ + ... + `alpha` * `ratio`^(iterations-1)^
    *
    * `alpha` = `epsilon` * (1 - `fraction`) / (1 - `fraction`^`iterations`^)
    *
    * @param epsilon   privacy budget
    * @param iterations number of iterations
    * @param fanout node division factor
    * @param alpha      coefficient of the exponential allocator
    * @return List[Double] of size iterations
    */
  var fanout: Int = 2

  /** Sets the fanout for the geometric allocator.
    *
    * @param fanout the node division factor
    * @return this instance of GeometricAllocator
    */
  def setFanout(fanout: Int): this.type = {
    this.fanout = fanout
    this
  }

  /** Creates an allocation list based on the geometric allocation algorithm.
    *
    * @param epsilon the privacy budget
    * @param iterations the number of iterations
    * @return a list of double values representing the geometric allocation
    */
  def createAllocation(
      epsilon: Double,
      iterations: Int
  ): List[Double] = {
    val ratio: Double = Math.pow(fanout, 1.0 / 3.0)
    val alpha: Double =
      epsilon * (ratio - 1) / (Math.pow(ratio, iterations) - 1)

    (0 until iterations)
      .map(i => alpha * Math.pow(ratio, i))
      .toList
  }

}
