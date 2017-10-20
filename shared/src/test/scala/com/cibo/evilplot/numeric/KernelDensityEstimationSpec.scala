package com.cibo.evilplot.numeric

import com.cibo.evilplot.numeric.KernelDensityEstimation._
import org.scalatest.{FunSpec, Matchers}

import scala.util.Random.nextDouble

class KernelDensityEstimationSpec extends FunSpec with Matchers {
  describe("KernelDensityEstimation") {
    it("should properly calculate probability densities from the normal distribution") {
      val doubles: Seq[Double] = Seq(0.383738345, 0.678363183, 0.870892648, 0.955542032, 0.739779717, 0.495273777,
        0.346604271, 0.971385358, 0.998761496, 0.222603808, 0.370077565, 0.081424898, 0.775284522, 0.005343148,
        0.470091059, 0.510200712, 0.361834899, 0.259037336, 0.806185498, 0.337947191)

      val densities: Seq[Double] = Seq(0.3706244, 0.3169451, 0.2730322, 0.2527211, 0.3034387, 0.3528943, 0.3756844,
        0.2488927, 0.2422704, 0.3891794, 0.3725376, 0.3976220, 0.2953862, 0.3989366, 0.3572100, 0.3502560, 0.3736631,
        0.3857797, 0.2882561, 0.3767993)

      (doubles zip densities).foreach { case (x, d) =>
        probabilityDensityInNormal(x) shouldEqual d +- 1e-5
      }
    }
    it("should properly calculate the matrix product A * B^T") {
      val a = Array(Array(0.9477583, 0.7026756, 0.0075461, 0.8175592),
        Array(0.5654393, 0.7140698, 0.5457264, 0.1904566),
        Array(0.8049051, 0.5844244, 0.5987555, 0.1988892),
        Array(0.6323643, 0.2691138, 0.7707659, 0.4891442),
        Array(0.2572372, 0.6319369, 0.2961405, 0.8173221))

      val b = Array(Array(0.95817, 0.72988, 0.39111, 0.51126),
        Array(0.42121, 0.98949, 0.41734, 0.76212),
        Array(0.55427, 0.47121, 0.73324, 0.42507),
        Array(0.98662, 0.85474, 0.22522, 0.52602),
        Array(0.94610, 0.54784, 0.21054, 0.92127))

      val answer = Array(Array(1.8419, 1.7207, 1.2095, 1.9674, 2.0364),
        Array(1.3738, 1.3176, 1.1310, 1.3913, 1.2165),
        Array(1.5337, 1.3188, 1.2451, 1.5331, 1.3910),
        Array(1.3539, 1.2271, 1.2504, 1.2848, 1.3586),
        Array(1.2414, 1.4801, 1.0049, 1.2906, 1.4049))
      val calculatedResult = KernelDensityEstimation.matrixMatrixTransposeMult(a, b).flatten.toSeq
      (calculatedResult zip answer.flatten.toSeq).foreach {
        case (calculated, actual) => calculated shouldEqual actual +- 0.003
      }
    }
  }

  describe("outer product calculation") {
    it("should calculate correctly") {
      val xs = Vector(-.3015008, 0.6520850)
      val ys = Vector(-.3033709, 0.6459446, 1.7718656)

      val outer = Array(Array(0.09146657, -0.1947528, -0.5342189), Array(-0.19782361, 0.4212108, 1.1554070))
      (outerProduct(xs.toArray, ys.toArray).flatten.toSeq zip outer.flatten.toSeq) foreach {
        case (calculated, actual) => calculated shouldBe actual +- 1e-6
      }
    }
  }

  describe("bandwidth estimation") {
    it("should always be non-negative") { // no ScalaCheck in scala.js ?
      (0 until 10) map (_ => Vector.fill(10)(nextDouble)) foreach (bandwidthEstimate(_) should be >= 0.0)
    }

    it("should be calculated properly for some sample vectors") {
      val xs = Vector(-1.06575970, 0.42420074, 0.02938372, 2.04974410, -1.63546604, 0.27436596,
        -0.90455302, 0.86564478, 1.68234299, 0.19371170)
      val ys = Vector(-0.7695360, 0.5401861, 0.3025197, 1.8889234, 1.1587218, -0.6744424, 0.9437049)

      bandwidthEstimate(xs) shouldBe 2.847659 +- 1e-6
      bandwidthEstimate(ys) shouldBe 2.652604 +- 1e-6
    }

    it("should reutrn NaN on a one element vector") {
      val xs = Vector(-1.045696)
      bandwidthEstimate(xs).isNaN shouldBe true
    }

    it("should return NaN on an empty vector") {
      bandwidthEstimate(Vector[Double]()).isNaN  shouldBe true
    }
  }
}
