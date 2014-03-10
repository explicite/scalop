package opt.unimodal

import org.scalatest.{ShouldMatchers, FunSuite}
import math.{sin, cos, tan, Pi}

/**
 * @author Jan 
 *         Date: 2/22/14
 */
class LagrangeMultiplierTest extends FunSuite with ShouldMatchers {
  val ε = 0.000001
  val γ = 0.01
  val N = 100

  test("Lagrange multiplier: min sin(x) over [π,2π]") {
    /*val lagrangeMultiplier = new LagrangeMultiplier(sin, (Pi, 2 * Pi))
    val min = 3.0 * Pi / 2.0
    lagrangeMultiplier(ε, γ, N)._1 should equal(min +- ε)*/
  }

  test("Lagrange multiplier: min cos(x) over [π,2π]") {
    /*val lagrangeMultiplier = new LagrangeMultiplier(cos, (Pi, 2 * Pi))
    val min = Pi
    lagrangeMultiplier(ε, γ, N)._1 should equal(min +- ε)*/
  }

  test("Lagrange multiplier: min tan(x) over [π,2π]") {
    /*val lagrangeMultiplier = new LagrangeMultiplier(tan, (Pi / 2, 3 / 2 * Pi))
    val min = Pi / 2
    lagrangeMultiplier(ε, γ, N)._1 should equal(min +- ε)*/
  }
}
