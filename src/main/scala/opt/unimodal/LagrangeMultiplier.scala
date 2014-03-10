package opt.unimodal

import math.abs

/**
 * In mathematical optimization, the method of Lagrange multipliers (named after Joseph Louis Lagrange) is a strategy
 * for finding the local maxima and minima of a function subject to equality constraints.
 * <p>For instance (see Figure 1), consider the optimization problem</p>
 * <p>maximize f(x, y)</p>
 * <p>subject to g(x, y) = c.</p>
 *
 * @param f function
 * @param d domain
 *
 * @author Jan
 *         Date: 2/22/14
 */
case class LagrangeMultiplier(f: Double => Double, d: (Double, Double)) {

  /**
   * Try find minimum with tolerance ε in N-steps when convergence is less than γ
   *
   * @param ε tolerance
   * @param γ convergence
   * @param N number of steps
   * @return  minimum (x,y)
   */
  def apply(ε: Double, γ: Double, N: Double): (Double, Double) = {
    val a: Double = d._1
    val b: Double = d._2

    var ai = a
    var bi = b
    var ci = (a + b) / 2.0

    def dx =
      0.5 * (f(ai) * ((ci * ci) - (bi * bi)) + f(ci) * ((bi * bi) - (ai * ai)) + f(bi) * ((ai * ai) - (ci * ci))) /
        ((f(ai) * (ci - bi)) + (f(ci) * (bi - ai)) + (f(bi) * (ai - ci)))

    var di = dx
    var i = 0

    repeat {

      di = dx

      if (ai < di && di < ci) {
        if (f(di) < f(ci)) {
          bi = ci
          ci = di
        } else {
          ai = di
        }
      } else if (ci < dx && dx < bi) {
        if (f(di) < f(ci)) {
          ai = ci
          ci = di
        } else {
          bi = di
        }
      } else
        throw new Error("algorithm converges!")

      i = i + 1
      if (i > N)
        throw new Error("cannot reach ε!")

    } until (bi - ai < ε || abs(dx - di) <= γ)

    val min = dx
    (min, f(min))
  }

  def repeat(body: => Unit) = new {
    def until(condition: => Boolean) = {
      do {
        body
      } while (!condition)
    }
  }
}
