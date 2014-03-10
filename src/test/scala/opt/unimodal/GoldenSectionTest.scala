package opt.unimodal

import org.scalatest.{ShouldMatchers, FunSuite}
import math.{sin, cos, tan, Pi}

/**
 * @author Jan Paw
 *         Date: 2/22/14
 */
class GoldenSectionTest extends FunSuite with ShouldMatchers {
  val ε = 0.000001
  val N = 50

  test("Golden section: min sin(x) over [π,2π]") {
    val goldenSection = new GoldenSection(sin, (Pi, 2 * Pi))
    val min = 3.0 * Pi / 2.0
    goldenSection(ε, N)._1 should equal(min +- ε)
  }

  test("Golden section: min cos(x) over [π,2π]") {
    val goldenSection = new GoldenSection(cos, (Pi, 2 * Pi))
    val min = Pi
    goldenSection(ε, N)._1 should equal(min +- ε)
  }

  test("Golden section: min tan(x) over [π,2π]") {
    val goldenSection = new GoldenSection(tan, (Pi / 2, 3 / 2 * Pi))
    val min = Pi / 2
    goldenSection(ε, N)._1 should equal(min +- ε)
  }
}
