package dp_multiq

import breeze.numerics.log2
import org.apache.commons.math3.transform.{
  DftNormalization,
  FastFourierTransformer,
  TransformType
}

import scala.math._
import scala.util.Random

/** JointExp method for computing multiple dp quantiles.
  */
object JointExp {

  /** Returns array of intervals of adjacent points.
    *
    * @param sortedData Nondecreasing array of data points, all in the [dataLow, dataHigh] range.
    * @param dataLow Lower bound for data.
    * @param dataHigh Upper bound for data.
    * @return An array of intervals of adjacent points.
    */
  def computeIntervals(
      sortedData: Array[Double],
      dataLow: Double,
      dataHigh: Double
  ): Array[Array[Double]] = {
    val intervals = (dataLow +: sortedData :+ dataHigh).sliding(2).toArray
    intervals.map(arr => Array(arr(0), arr(1)))
  }

  /** Computes two-dimensional array log_phi.
    *
    * @param dataIntervals Array of intervals of adjacent points from computeIntervals.
    * @param qs Increasing array of quantiles in [0,1].
    * @param eps Privacy parameter epsilon.
    * @param swap If true, uses swap dp sensitivity, otherwise uses add-remove.
    * @return Array log_phi.
    */
  def computeLogPhi(
      dataIntervals: Array[Array[Double]],
      qs: Array[Double],
      eps: Double,
      swap: Boolean
  ): Array[Array[Double]] = {
    val numDataIntervals = dataIntervals.length
    val originalDataSize = numDataIntervals - 1
    val sensitivity =
      if (swap)
        2.0
      else if (qs.length == 1)
        2.0 * (1 - Math.min(qs(0), 1 - qs(0)))
      else
        2.0 * (1 - Math.min(
          qs(0),
          Math.min(
            qs.tail.zip(qs.init).map { case (a, b) => a - b }.min,
            1 - qs.last
          )
        ))

    val epsTerm = -(eps / (2.0 * sensitivity))
    val gaps = (0 until numDataIntervals).toArray

    val targetNs = (qs :+ 1.0).zip(0.0 +: qs).map { case (q1, q0) =>
      (q1 - q0) * originalDataSize
    }

    val logPhi = Array.ofDim[Double](numDataIntervals, targetNs.length)

    for (i <- 0 until numDataIntervals) {
      for (j <- targetNs.indices) {
        logPhi(i)(j) = epsTerm * Math.abs(gaps(i) - targetNs(j))
      }
    }

    logPhi
  }

  def padArrayToSize(arr: Array[Double], p: Int): Array[Double] = {
    if (arr.length >= p) {
      // If the array is already of size p or larger, return it as is
      arr
    } else {
      // Create a new array of size p filled with 0s
      val paddedArray = new Array[Double](p)

      // Copy the elements from the original array to the padded array
      Array.copy(arr, 0, paddedArray, 0, arr.length)

      paddedArray
    }
  }

  /** Multiplies a log-space vector by a lower triangular Toeplitz matrix.
    *
    * @param c First column of the Toeplitz matrix (in log space).
    * @param x Vector to be multiplied (in log space).
    * @return Let T denote the lower triangular Toeplitz matrix whose first column is
    *         given by exp(c); then the vector returned by this function is log(T *
    *         exp(x)). The multiplication is done using FFTs for efficiency, and care is
    *         taken to avoid overflow during exponentiation.
    */
  def logdotexpToeplitzLt(c: Array[Double], x: Array[Double]): Array[Double] = {
    val maxC = c.max
    val maxX = x.max
    val expC = c.map(_ - maxC).map(exp)
    val expX = x.map(_ - maxX).map(exp)
    val n = x.length
    val p = pow(2, ceil(log2(2 * n - 1))).toInt
    val expCp = padArrayToSize(expC, p)
    val expXp = padArrayToSize(expX, p)

    val fft = new FastFourierTransformer(DftNormalization.STANDARD)

    val fft_exp_c = fft.transform(expCp, TransformType.FORWARD)
    val fft_exp_x = fft.transform(expXp, TransformType.FORWARD)

    val multiplication =
      fft_exp_c.zip(fft_exp_x).map(x => x._1.multiply(x._2))

    val y = fft
      .transform(multiplication, TransformType.INVERSE)
      .take(n)
      .map(x => log(max(x.getReal, 1e-300)) + maxC + maxX )
    y
  }

  /** Compute the three-dimensional array log_alpha.
    *
    * @param dataIntervals Array of intervals of adjacent points.
    * @param logPhi        Array from computeLogPhi.
    * @param qs            Increasing array of quantiles in (0, 1).
    * @return Array log_alpha(a, b, c) where a and c index over quantiles and b represents interval repeats.
    */
  def computeLogAlpha(
      dataIntervals: Array[Array[Double]],
      logPhi: Array[Array[Double]],
      qs: Array[Double]
  ): Array[Array[Array[Double]]] = {
    val numIntervals = dataIntervals.length
    val numQuantiles = qs.length

    val dataIntervalsLogSizes =
      dataIntervals.map(interval => log(interval(1) - interval(0)))

    var logAlpha = Array.ofDim[Double](numQuantiles, numIntervals, numQuantiles)

    logAlpha = logAlpha.map(_.map(_.map(x => log(x))))

    for (j <- logAlpha.head.indices)
      logAlpha(0)(j)(0) = logPhi(j)(0) + dataIntervalsLogSizes(j)

    var disallowRepeat = Array.ofDim[Double](numIntervals)
    disallowRepeat(0) = Double.NegativeInfinity

    for (j <- 1 until numQuantiles) {

      val logHatAlpha =
        logAlpha(j - 1).map(row => math.log(row.map(math.exp).sum))

      val toeplitzLtMat = logdotexpToeplitzLt(
        logPhi.zipWithIndex.map { case (row, idx) =>
          row(j) + disallowRepeat(idx)
        },
        logHatAlpha
      )

      for (i <- 0 until numIntervals)
        logAlpha(j)(i)(0) = dataIntervalsLogSizes(i) + toeplitzLtMat(i)

      logAlpha(j)(0)(0) = Double.NegativeInfinity
      val logArrange = (1 to j).toArray.map(x => log(x.toDouble + 1))

      for (i <- 0 until numIntervals)
        for (k <- 1 to j)
          logAlpha(j)(i)(k) = logPhi(0)(j) + dataIntervalsLogSizes(i) +
            logAlpha(j - 1)(i)(k - 1) - logArrange(k - 1)
    }
    logAlpha
  }

  /** Sample final quantile estimates given log_alpha and log_phi.
    *
    * @param logAlpha      Array from computeLogAlpha.
    * @param dataIntervals Array of intervals of adjacent points.
    * @param logPhi        Array from computeLogPhi.
    * @param qs            Increasing array of quantiles in (0, 1).
    * @return Array of quantile estimates.
    */
  def sampleJointExp(
      logAlpha: Array[Array[Array[Double]]],
      dataIntervals: Array[Array[Double]],
      logPhi: Array[Array[Double]],
      qs: Array[Double]
  ): Array[Double] = {
    val numIntervals = dataIntervals.length
    val numQuantiles = qs.length
    val outputs = Array.ofDim[Double](numQuantiles)
    var lastI = numIntervals - 1
    var j = numQuantiles - 1
    var repeats = 0

    while (j >= 0) {
      val logDist = Array.ofDim[Double](lastI + 1, numQuantiles)

      for (k <- 0 until numQuantiles)
        for (i <- 0 until lastI + 1)
          logDist(i)(k) = logAlpha(j)(i)(k) + logPhi(lastI - i)(j + 1)

      if (j < numQuantiles - 1)
        for (i <- 0 until numQuantiles)
          logDist(lastI)(i) = Double.NegativeInfinity

      var (i, k) = IndExp.racingSample(logDist)

      repeats += k
      k = k + 1

      for (j2 <- j - k + 1 to j)
        outputs(j2) = Random.nextDouble() *
          (dataIntervals(i)(1) - dataIntervals(i)(0)) + dataIntervals(i)(0)

      j -= k
      lastI = i
    }
    outputs.sorted
  }

  /** Computes eps-differentially private quantile estimates for qs.
    *
    * @param sortedData Sorted array of data points.
    * @param dataLow    Lower bound for data.
    * @param dataHigh   Upper bound for data.
    * @param qs         Increasing array of quantiles in (0,1).
    * @param eps        Privacy parameter epsilon.
    * @param swap       If true, uses swap dp sensitivity, otherwise uses add-remove.
    * @return Array of quantile estimates.
    */
  def jointExp(
      sortedData: Array[Double],
      dataLow: Double,
      dataHigh: Double,
      qs: Array[Double],
      eps: Double,
      swap: Boolean
  ): Array[Double] = {
    val clippedData =
      sortedData.map(data => math.max(dataLow, math.min(data, dataHigh)))
    val dataIntervals = computeIntervals(clippedData, dataLow, dataHigh)
    val logPhi = computeLogPhi(dataIntervals, qs, eps, swap)
    val logAlpha = computeLogAlpha(dataIntervals, logPhi, qs)
    sampleJointExp(logAlpha, dataIntervals, logPhi, qs)
  }
  def main(args: Array[String]): Unit = {
    val sortedData =
      Array(1.0, 2.0, 3.0, 4.0, 5.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0)
    val dataLow = 0.0
    val dataHigh = 15.0
    val qs = Array(0.25, 0.5, 0.75)
    val dividedEps = 0.1
    val swap = true

    jointExp(sortedData, dataLow, dataHigh, qs, dividedEps, swap).foreach(
      println
    )
  }
}
