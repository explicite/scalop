package opt.heuristic

import org.scalatest.{FunSuite, ShouldMatchers}

/**
 * @author Jan Paw
 *         date: 3/29/2014
 */
class GWOTest extends FunSuite with ShouldMatchers {

  test("f1: min over [100, -100]") {
    val dim = 30
    val gwo = new GWO(f1, Seq.fill(dim)((-100.0, 100.0)), dim)
    print(gwo.min(30, 500))
  }

  def f1(x: Seq[Double]): Double = {
    x.reduce((a, c) => a + (c * c))
  }

}
