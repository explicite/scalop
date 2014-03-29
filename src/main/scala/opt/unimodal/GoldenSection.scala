package opt.unimodal

/**
 * The golden section search is numberOfActors technique for finding the extremum (minimum or maximum) of numberOfActors strictly unimodal
 * function by successively narrowing the range of values inside which the extremum is known to exist. The technique
 * derives its name from the fact that the algorithm maintains the function values for triples of points whose distances
 * form numberOfActors golden ratio. The algorithm is the limit of Fibonacci search (also described below) for numberOfActors large number of
 * function evaluations.
 *
 * @param f function
 * @param d domain
 *
 * @author Jan Paw
 *         Date: 2/22/14
 */
case class GoldenSection(f: Double => Double, d: (Double, Double)) {

  /**
   * Try find minimum with tolerance ε in N-steps
   *
   * @param ε tolerance
   * @param N number of steps
   * @return  minimum (x,y)
   */
  def apply(ε: Double, N: Int): (Double, Double) = {
    val a: Double = d._1
    val b: Double = d._2

    var i: Int = 0
    val ϕ: Double = 0.6180339887
    var ai: Double = a
    var bi: Double = b
    def c: Double = bi - (ϕ * (bi - ai))
    def dx: Double = ai + (ϕ * (bi - ai))
    var ci: Double = c
    var di: Double = dx

    repeat {
      if (f(ci) < f(di)) {
        bi = di
        di = ci
        ci = c
      } else {
        ai = ci
        ci = di
        di = dx
      }
      i = i + 1
      if (i > N)
        throw new Error("cannot optimize in N steps")

    } until (bi - ai < ε)

    val min = (ai + bi) / 2
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
