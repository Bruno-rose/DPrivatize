package dp_multiq

import breeze.numerics.log2
import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.transform.{
  DftNormalization,
  FastFourierTransformer,
  TransformType
}

import scala.math._

object FFTExample {
  def padToNextPowerOf2(array: Array[Double]): Array[Double] = {
    val currentLength = array.length
    val nextPowerOf2 = pow(2, ceil(log2(2 * currentLength - 1))).toInt
    val paddedArray = new Array[Double](nextPowerOf2)

    // Copy the original array into the padded array
    System.arraycopy(array, 0, paddedArray, 0, currentLength)

    paddedArray
  }

  def main(args: Array[String]): Unit = {
    // Create a sample real-valued input signal
    val signal1 = Array(1.0, 2.0, 1.0, -1.0, 0.5)
    val signal2 = Array(9.0, -2.0, 3.0, 3.0, -4)

    val n_length = signal1.length

    val paddedSignal1 = padToNextPowerOf2(signal1)
    val paddedSignal2 = padToNextPowerOf2(signal2)

    // Perform the real-to-complex FFT using FastFourierTransformer
    val fft = new FastFourierTransformer(DftNormalization.STANDARD)

    val fftResult1 = fft.transform(paddedSignal1, TransformType.FORWARD)
    val fftResult2 = fft.transform(paddedSignal2, TransformType.FORWARD)
    val multiplication =
      fftResult1.zip(fftResult2).map(x => x._1.multiply(x._2))

    val reconstructedSignal = fft
      .transform(multiplication, TransformType.INVERSE)
      .take(n_length)

    println("\nReconstructed Signal:")
    println(reconstructedSignal.map(_.getReal).mkString(" "))

  }
}
