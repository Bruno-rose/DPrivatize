package grid.epsilon

/** A class that implements linear allocation using different modes of computation.
  */
class LinearAllocator extends EpsilonAllocator {

  /** We use the formula:
    * `epsilon` = SUM_i slope * i + intercept, for i = `1` to iterations
    *
    * @param epsilon the target value to achieve through linear allocation
    * @param iterations the number of iterations to compute the linear allocation
    * @param coefficient the coefficient to use in the linear allocation computation
    * @param mode the mode of computation, either "slope" or "intercept"
    * @return a list of double values representing the linear allocation
    * @throws Exception if the mode is not recognized
    */

  var coefficient: Double = 0.0001

  var mode: String = "intercept"

  def setCoefficient(coefficient: Double): this.type = {
    this.coefficient = coefficient
    this
  }

  def setMode(mode: String): this.type = {
    this.mode = mode
    this
  }

  def createAllocation(
      epsilon: Double,
      iterations: Int
  ): List[Double] = {
    if (mode == "slope")
      createAllocationSlope(epsilon, iterations, coefficient)
    else if (mode == "intercept")
      createAllocationIntercept(epsilon, iterations, coefficient)
    else
      throw new Exception("mode not recognized")
  }

  /** Computes linear allocation using slope as a parameter.
    *
    * @param epsilon the target value to achieve through linear allocation
    * @param iterations the number of iterations to compute the linear allocation
    * @param slope the slope to use in the linear allocation computation
    * @return a list of double values representing the linear allocation
    * @throws Exception if the computed intercept is non-positive
    */
  def createAllocationSlope(
      epsilon: Double,
      iterations: Int,
      slope: Double
  ): List[Double] = {
    val intercept =
      (epsilon - slope * iterations * (iterations + 1) / 2) / iterations
    if (intercept <= 0)
      throw new Exception("intercept is non-positive")
    createAllocationSlopeIntercept(iterations, slope, intercept)
  }

  /** Computes linear allocation using intercept as a parameter.
    *
    * @param epsilon the target value to achieve through linear allocation
    * @param iterations the number of iterations to compute the linear allocation
    * @param intercept the intercept to use in the linear allocation computation
    * @return a list of double values representing the linear allocation
    * @throws Exception if the computed slope is non-positive
    */
  def createAllocationIntercept(
      epsilon: Double,
      iterations: Int,
      intercept: Double
  ): List[Double] = {
    val slope =
      (epsilon - iterations * intercept) * 2.0 / (iterations * (iterations + 1))
    if (slope <= 0)
      throw new Exception("slope is non-positive")
    createAllocationSlopeIntercept(iterations, slope, intercept)
  }

  /** Computes linear allocation using both slope and intercept as parameters.
    *
    * @param iterations the number of iterations to compute the linear allocation
    * @param slope the slope to use in the linear allocation computation
    * @param intercept the intercept to use in the linear allocation computation
    * @return a list of double values representing the linear allocation
    */
  def createAllocationSlopeIntercept(
      iterations: Int,
      slope: Double,
      intercept: Double
  ): List[Double] = {
    (1 to iterations).map(i => i * slope + intercept).toList
  }
}
