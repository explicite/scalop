package opt

import scala.math._

/**
 * @author Jan Paw
 *         Date: 4/10/2014
 */
object Function {
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

  def BoothFunction(xs: Seq[Double]) = {
    val x = xs(0)
    val y = xs(1)
    ((x + (2 * y) - 7) * (x + (2 * y) - 7)) + (((2 * x) + y - 5) * ((2 * x) + y - 5))
  }

  def BukinFunction(xs: Seq[Double]) = {
    val x = xs(0)
    val y = xs(1)
    100 * sqrt(abs(y - (0.01 * x * x))) + (0.01 * abs(x + 10))
  }

  def LeviFunction(xs: Seq[Double]) = {
    val x = xs(0)
    val y = xs(1)
    (sin(3 * Pi * x) * sin(3 * Pi * x)) + ((x - 1) * (x - 1) * (1 + (sin(3 * Pi * y) * sin(3 * Pi * y)))) + (((y - 1) * (y - 1)) * (1 + (sin(2 * Pi * y) * sin(2 * Pi * y))))
  }

}
