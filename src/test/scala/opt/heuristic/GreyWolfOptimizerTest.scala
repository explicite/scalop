package opt.heuristic

import org.scalatest.{FunSuite, ShouldMatchers}
import scala.math.{sqrt, sin, cos, Pi}

/**
 * @author Jan Paw
 *         date: 3/29/2014
 */
class GreyWolfOptimizerTest extends FunSuite with ShouldMatchers {

  test("f1: min over [-100, 100]") {
    val dim = 30
    val min = f1(Seq.fill(dim)(0d))
    val gwo = new GreyWolfOptimizer(f1, Seq.fill(dim)((-100d, 100d)))
    f1(gwo.min(dim, 500)) should equal(min)
  }

  test("f2: min over [-5, 5]") {
    val dim = 30
    val min = f2(Seq.fill(dim)(0d))
    val gwo = new GreyWolfOptimizer(f2, Seq.fill(dim)((-5d, 5d)))
    f2(gwo.min(dim, 500)) should equal(min)
  }

  test("f3: min over [-1, 1]") {
    val dim = 30
    val min = f3(Seq.fill(dim)(0d))
    val gwo = new GreyWolfOptimizer(f3, Seq.fill(dim)((-100d, 100d)))
    f3(gwo.min(dim, 500)) should equal(min)
  }

  test("f4: max over [6, 9]") {
    val min = f4(Seq((2 * Pi) + (Pi / 2), 2 * Pi))
    val gwo = new GreyWolfOptimizer(f4, Seq((6d, 9d), (6d, 9d)))
    f4(gwo.max(500, 1000)) should equal(min +- 0.05)
  }

  def f1(x: Seq[Double]): Double = {
    x.reduce((a, c) => a + (c * c))
  }

  def f2(x: Seq[Double]): Double = {
    x.reduce((a, c) => (a * a) - (c * c))
  }

  def f3(x: Seq[Double]): Double = {
    2 * sqrt(sqrt((x(0) * x(0)) + (x(1) * x(1))))
  }

  def f4(x: Seq[Double]): Double = {
    sin(x(0)) + cos(x(1))
  }

}
