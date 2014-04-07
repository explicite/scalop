package opt

import scala.math._

/**
 * @author Jan Paw
 *         Date: 4/7/2014
 */
package object heuristic {

  object functions {
    def AckleysFunction(xs: Seq[Double]): Double = {
      val x = xs(0)
      val y = xs(1)
      -20 * exp(-0.2 * sqrt(0.5 * ((x * x) + (y * y)))) - exp(0.5 * (cos(2 * Pi * x) + cos(2 * Pi * y))) + 20 + E
    }

    def SphereFunction(xs: Seq[Double]): Double = {
      xs.foldLeft(0d)((res, x) => res + (x * x))
    }

    def BealesFunction(xs: Seq[Double]): Double = {
      val x = xs(0)
      val y = xs(1)
      ((1.5 - x + (x * y)) * (1.5 - x + (x * y))) +
        ((2.25 - x + (x * y * y)) * (2.25 - x + (x * y * y))) +
        ((2.625 - x + (x * y * y * y)) * (2.625 - x + (x * y * y * y)))
    }

    def RastriginFunction(xs: Seq[Double]): Double = {
      (10d * xs.length) + xs.foldLeft(0d)((res, x) => res + ((x * x) - (10d * cos(2d * Pi * x))))
    }

    def EasomFunction(xs: Seq[Double]): Double = {
      -cos(xs(0)) * cos(xs(1)) * exp(-(((xs(0) - Pi) * (xs(0) - Pi)) + ((xs(1) - Pi) * (xs(1) - Pi))))
    }

    def McCormicFunction(xs: Seq[Double]): Double = {
      sin(xs(0) + xs(1)) + ((xs(0) - xs(1)) * (xs(0) - xs(1))) - (1.5 * xs(0)) + (2.5 * xs(1)) + 1d
    }
  }

}
