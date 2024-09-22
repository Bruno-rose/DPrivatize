package dp_multiq

import scala.math._
import scala.util.Random

object QuantileMethods {

  /**
   * Returns the index of the specified quantile in a sorted dataset of n elements.
   *
   * @param n       Size of the sorted dataset.
   * @param quantile A value in [0, 1] indicating the desired quantile.
   * @return Index of the specified quantile. If the quantile is between points at indices i and i+1, returns i.
   */
  def quantileIndex(n: Int, quantile: Double): Int = {
    floor((n - 1) * quantile).toInt
  }

  /**
   * Returns an array of quantile estimates for the specified quantiles.
   *
   * @param data A dataset sorted in increasing order.
   * @param qs   An array of increasing quantiles in [0, 1].
   * @return An array of quantile estimates corresponding to the input quantiles.
   */
  def quantiles(data: Array[Double], qs: Array[Double]): Array[Double] = {
    data.zip(qs).map { case (d, q) =>
      quantile(data, q)
    }
  }

  /**
   * Returns the quantile estimate for a specific quantile.
   *
   * @param data    A dataset sorted in increasing order.
   * @param quantile A quantile value in [0, 1].
   * @return The quantile estimate for the specified quantile.
   */
  def quantile(data: Array[Double], quantile: Double): Double = {
    val index = quantileIndex(data.length, quantile)
    data(index)
  }

  /**
   * Returns the average number of data points between true and estimated quantiles.
   *
   * @param sortedData     A dataset sorted in increasing order.
   * @param trueQuantiles  Ground truth quantiles.
   * @param estQuantiles   Estimated quantiles.
   * @return The number of data points strictly between true_quantiles[j] and est_quantiles[j], averaged over all j.
   */
  def misclassifiedPointsError(sortedData: Array[Double], trueQuantiles: Array[Double], estQuantiles: Array[Double]): Double = {
    val numQuantiles = trueQuantiles.length
    val totalMissed = (0 until numQuantiles).map { qIdx =>
      abs(sortedData.count(_ > trueQuantiles(qIdx)) - sortedData.count(_ > estQuantiles(qIdx)))
    }.sum.toDouble
    totalMissed / numQuantiles
  }

  /**
   * Returns the mean distance between the true and estimated quantiles.
   *
   * @param trueQuantiles Ground truth quantiles.
   * @param estQuantiles  Estimated quantiles.
   * @return The mean distance between the true and estimated quantiles.
   */
  def distanceError(trueQuantiles: Array[Double], estQuantiles: Array[Double]): Double = {
    trueQuantiles.zip(estQuantiles).map { case (trueQ, estQ) =>
      abs(trueQ - estQ)
    }.sum / trueQuantiles.length.toDouble
  }

  /**
   * Generates numSamples IID Gaussian samples in increasing order.
   *
   * @param numSamples Number of samples to return.
   * @param mean       Mean of the Gaussian distribution to sample.
   * @param stddev     Standard deviation of the Gaussian distribution to sample.
   * @return An array of Gaussian samples.
   */
  def genGaussian(numSamples: Int, mean: Double, stddev: Double): Array[Double] = {
    val random = new Random()
    val samples = (0 until numSamples).map { _ =>
      random.nextGaussian() * stddev + mean
    }
    samples.sorted.toArray
  }

  /**
   * Generates numSamples IID uniform samples in increasing order.
   *
   * @param numSamples Number of samples to return.
   * @param dataLow    Lower bound of the uniform distribution to sample.
   * @param dataHigh   Upper bound of the uniform distribution to sample.
   * @return An array of uniform samples.
   */
  def genUniform(numSamples: Int, dataLow: Double, dataHigh: Double): Array[Double] = {
    val random = new Random()
    val samples = (0 until numSamples).map { _ =>
      random.nextDouble() * (dataHigh - dataLow) + dataLow
    }
    samples.sorted.toArray
  }

}
