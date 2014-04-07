package opt.heuristic

import org.scalatest.{FunSuite, ShouldMatchers}
import scala.math.Pi
import functions._

/**
 * @author Jan Paw
 *         Date: 3/29/2014
 */
class GreyWolfOptimizerTest extends FunSuite with ShouldMatchers {
  val wolfs = 300
  val iterations = 1000
  val dim = 3
  val ε = 0.001

  test("Ackley's function: min over [-5, 5]") {
    val min = AckleysFunction(Seq(0, 0))
    val gwo = new GreyWolfOptimizer(AckleysFunction, Seq((-5d, 5d), (-5d, 5d)))
    AckleysFunction(gwo.min(wolfs, iterations)) should equal(min +- ε)
  }

  test("Sphere function: min over [-5, 5]") {
    val min = SphereFunction(Seq.fill(dim)(0d))
    val gwo = new GreyWolfOptimizer(SphereFunction, Seq.fill(dim)((-5d, 5d)))
    SphereFunction(gwo.min(wolfs * dim, iterations * dim)) should equal(min +- ε)
  }

  test("Beale's function: min over [-4.5, 4.5]") {
    val min = BealesFunction(Seq(3d, 0.5))
    val gwo = new GreyWolfOptimizer(BealesFunction, Seq((-4.5, 4.5), (-4.5, 4.5)))
    BealesFunction(gwo.min(wolfs, iterations)) should equal(min +- ε)
  }

  test("Rastrigin function: min [-5.12, 5.12]") {
    val min = RastriginFunction(Seq.fill(dim)(0d))
    val gwo = new GreyWolfOptimizer(RastriginFunction, Seq.fill(dim)((-5.12, 5.12)))
    RastriginFunction(gwo.min(wolfs * dim, iterations * dim)) should equal(min +- ε)
  }

  test("Eosom function: min [-100, 100]") {
    val min = EasomFunction(Seq(Pi, Pi))
    val gwo = new GreyWolfOptimizer(EasomFunction, Seq((-100d, 100d), (-100d, 100d)))
    EasomFunction(gwo.min(wolfs, iterations)) should equal(min +- ε)
  }

  test("McCormick function: min [-1.5, 4] [-3, 4]") {
    val min = McCormicFunction(Seq(-0.54719, -1.54719))
    val gwo = new GreyWolfOptimizer(McCormicFunction, Seq((-1.5, 4d), (-3d, 4d)))
    McCormicFunction(gwo.min(wolfs, iterations)) should equal(min +- ε)
  }
}
