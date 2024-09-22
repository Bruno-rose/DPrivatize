package dp_multiq

import scala.util.Random

/** IndExp method for computing differentially private quantiles.
  *
  * Algorithm 2 from the paper "Privacy-preserving Statistical Estimation with
  * Optimal Convergence Rates" by Smith (STOC 2011,
  * http://cs-people.bu.edu/ads22/pubs/2011/stoc194-smith.pdf) describes the
  * subroutine used to compute a single quantile. Theorem 3 from the paper "Optimal
  * Differential Privacy Composition for Exponential Mechanisms and the Cost of
  * Adaptivity" by Dong, Durfee, and Rogers (ICML 2020,
  * https://arxiv.org/pdf/1909.13830.pdf) describes the composition used for the
  * approximate DP variant of IndExp.
  */
object IndExp {

  /** Numerically stable method for sampling from an exponential distribution.
    *
    * @param logTerms Array of terms of form log(coefficient) - (exponent term).
    * @return A sample from the exponential distribution determined by terms.
    */
  def racingSample(logTerms: Array[Double]): Int = {
    val randomValues =
      Array.fill(logTerms.length)(Math.log(-Math.log(Random.nextDouble())))
    logTerms.indices.minBy(i => randomValues(i) - logTerms(i))
  }

  def racingSample(logTerms: Array[Array[Double]]): (Int, Int) = {
    val randomValues =
      Array.fill(logTerms.length, logTerms.head.length)(
        Math.log(-Math.log(Random.nextDouble()))
      )

    (for {
      i <- randomValues.indices
      j <- randomValues.head.indices
    } yield (i, j)).minBy(ind =>
      randomValues(ind._1)(ind._2) - logTerms(ind._1)(ind._2)
    )
  }

  /** Returns eps-differentially private collection of quantile estimates for qs.
    *
    * @param sortedData Array of data points sorted in increasing order.
    * @param dataLow Lower limit for any differentially private quantile output value.
    * @param dataHigh Upper limit for any differentially private quantile output value.
    * @param qs Increasing array of quantiles in [0, 1].
    * @param dividedEps Privacy parameter epsilon for each estimated quantile.
    * @param swap If true, uses swap dp sensitivity, otherwise uses add-remove.
    * @return Array of eps-differentially private quantile estimates.
    */
  def indExp(
      sortedData: Array[Double],
      dataLow: Double,
      dataHigh: Double,
      qs: Array[Double],
      dividedEps: Double,
      swap: Boolean
  ): Array[Double] = {
    val numQuantiles = qs.length
    val outputs = Array.ofDim[Double](numQuantiles)
    val clippedData =
      sortedData.map(x => Math.max(dataLow, Math.min(dataHigh, x)))

    val dataSize = clippedData.length

    val sortedDataWithBounds = dataLow +: clippedData :+ dataHigh

    val dataGaps = (0 until sortedDataWithBounds.length - 1).map(i =>
      sortedDataWithBounds(i + 1) - sortedDataWithBounds(i)
    )
    for (qIdx <- 0 until numQuantiles) {
      val quantile = qs(qIdx)
      val sensitivity = if (swap) 1.0 else Math.max(quantile, 1 - quantile)

      val logTerms = dataGaps.zipWithIndex.map { case (gap, idx) =>
        Math.log(gap) +
          (dividedEps / (-2.0 * sensitivity)) *
          Math.abs(idx - quantile * dataSize)
      }.toArray

      val idxLeft = racingSample(logTerms)

      outputs(qIdx) = sortedDataWithBounds(idxLeft) + Random
        .nextDouble() * (sortedDataWithBounds(
        idxLeft + 1
      ) - sortedDataWithBounds(idxLeft))
    }
    outputs.sorted
  }

  def main(args: Array[String]): Unit = {
    // Example usage
    val sortedData = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val dataLow = 0.0
    val dataHigh = 6.0
    val qs = Array(0.25, 0.5, 0.75)
    val dividedEps = 1
    val swap = true
    val result = indExp(sortedData, dataLow, dataHigh, qs, dividedEps, swap)
    println(result.mkString(", "))
  }
}
