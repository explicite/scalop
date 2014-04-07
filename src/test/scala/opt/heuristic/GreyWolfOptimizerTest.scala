package opt.heuristic

import org.scalatest.{FunSuite, ShouldMatchers}
import scala.math.{sqrt, sin, cos, exp, Pi, E}

/**
 * @author Jan Paw
 *         date: 3/29/2014
 */
class GreyWolfOptimizerTest extends FunSuite with ShouldMatchers {
  val wolfs = 300
  val iterations = 1000
  val dim = 3
  val ε = 0.001

  test("Ackley's function: min over [-5, 5]") {
    val min = ackleysFunction(Seq(0, 0))
    val gwo = new GreyWolfOptimizer(ackleysFunction, Seq((-5d, 5d), (-5d, 5d)))
    ackleysFunction(gwo.min(wolfs, iterations)) should equal(min +- ε)
  }

  test("Rastrigin function: min [-5.12, 5.12]") {
    val min = rastriginFunction(Seq.fill(dim)(0d))
    val gwo = new GreyWolfOptimizer(rastriginFunction, Seq.fill(dim)((-5.12, 5.12)))
    rastriginFunction(gwo.min(wolfs * dim, iterations * dim)) should equal(min +- ε)
  }

  test("Eosom function: min [-100, 100]") {
    val min = easomFunction(Seq(Pi, Pi))
    val gwo = new GreyWolfOptimizer(easomFunction, Seq((-100d, 100d), (-100d, 100d)))
    easomFunction(gwo.min(wolfs, iterations)) should equal(min +- ε)
  }

  test("McCormick function: min [-1.5, 4] [-3, 4]") {
    val min = mcCormicFunction(Seq(-0.54719, -1.54719))
    val gwo = new GreyWolfOptimizer(mcCormicFunction, Seq((-1.5, 4d), (-3d, 4d)))
    mcCormicFunction(gwo.min(wolfs, iterations)) should equal(min +- ε)
  }

  def ackleysFunction(xs: Seq[Double]): Double = {
    val x = xs(0)
    val y = xs(1)
    -20 * exp(-0.2 * sqrt(0.5 * ((x * x) + (y * y)))) - exp(0.5 * (cos(2 * Pi * x) + cos(2 * Pi * y))) + 20 + E
  }

  def rastriginFunction(xs: Seq[Double]): Double = {
    (10d * xs.length) + xs.foldLeft(0d)((res, x) => res + ((x * x) - (10d * cos(2d * Pi * x))))
  }

  def easomFunction(xs: Seq[Double]): Double = {
    -cos(xs(0)) * cos(xs(1)) * exp(-(((xs(0) - Pi) * (xs(0) - Pi)) + ((xs(1) - Pi) * (xs(1) - Pi))))
  }

  def mcCormicFunction(xs: Seq[Double]): Double = {
    sin(xs(0) + xs(1)) + ((xs(0) - xs(1)) * (xs(0) - xs(1))) - (1.5 * xs(0)) + (2.5 * xs(1)) + 1d
  }

}
