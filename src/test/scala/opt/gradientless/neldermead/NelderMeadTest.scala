package opt.gradientless.neldermead

import opt.Function._
import test.BaseTest
import scala.math._

class NelderMeadTest extends BaseTest {
  behavior of "NelderMead"
  val dim = 3

  it must "find min for Ackley's function" in {
    val ε = 0.001
    val min = AckleysFunction(Seq(0d, 0d))
    val nelderMead = NelderMead(AckleysFunction)
    val points = Seq(Seq(-5d, -5d), Seq(-5d, 5d), Seq(2.5, 5d))
    AckleysFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }
  it must "find min for Sphere function" in {
    val ε = 0.001
    val min = SphereFunction(Seq.fill(dim)(0d))
    val nelderMead = NelderMead(SphereFunction)
    val points = Seq(Seq(-5d, -5d), Seq(-5d, 5d), Seq(2.5, 5d))
    SphereFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

  it must "find min for Beale's function" in {
    val ε = 0.001
    val min = BealesFunction(Seq(3d, 0.5))
    val nelderMead = NelderMead(BealesFunction)
    val points = Seq(Seq(-4.5, -4.5), Seq(-4.5, 4.5), Seq(2d, 4.5))
    BealesFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

  it must "find min for Rastrigin function" in {
    val ε = 0.001
    val min = RastriginFunction(Seq.fill(dim)(0d))
    val nelderMead = NelderMead(RastriginFunction)
    val points = Seq(Seq(-5d, -5d), Seq(-5d, 5d), Seq(2d, 5d))
    RastriginFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

  ignore must "find min for Eosom function" in {
    val ε = 0.001
    val min = EasomFunction(Seq(Pi, Pi))
    val nelderMead = NelderMead(EasomFunction, α = 0.8, β = 0.3, γ = 1.9, δ = 0.4)
    val points = Seq(Seq(-20d, 80d), Seq(-100d, 10d), Seq(-100d, 100d))
    EasomFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

  ignore must "find min for McCormick function" in {
    val ε = 0.001
    val min = McCormicFunction(Seq(-0.54719, -1.54719))
    val nelderMead = NelderMead(McCormicFunction)
    val points = Seq(Seq(-1d, 1d), Seq(-0.5, 2d), Seq(-2d, -1.5))
    McCormicFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

  it must "find min for Booth function" in {
    val ε = 0.001
    val min = BoothFunction(Seq(1, 3))
    val nelderMead = NelderMead(BoothFunction)
    val points = Seq(Seq(-5d, 5d), Seq(5d, 5d), Seq(2d, -5d))
    BoothFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

  ignore must "find min for Bukin function" in {
    val ε = 0.001
    val min = BukinFunction(Seq(-10, 1))
    val nelderMead = NelderMead(BukinFunction, α = 0.6, β = 0.3, γ = 1.5, δ = 0.3)
    val points = Seq(Seq(-5d, -3d), Seq(-10d, 3d), Seq(-10d, 0d))
    BukinFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

  it must "find min for Levi function" in {
    val ε = 0.001
    val min = LeviFunction(Seq(1, 1))
    val nelderMead = NelderMead(LeviFunction, α = 1.0, β = 0.7, γ = 1.5, δ = 0.5)
    val points = Seq(Seq(-5d, -3d), Seq(-10d, 3d), Seq(-10d, 10d))
    LeviFunction(nelderMead.minimize(points, ε)) should equal(min +- ε)
  }

}