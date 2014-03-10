package opt.unimodal

import scala.annotation.tailrec

/**
 * Fibonacci Line Search algorithm to find the maximum of a unimodal function, f(x) , over an interval, d._1 < x < d._2
 * The program calculates the number of iterations required to insure the final interval is within the user-specified
 * tolerance. This is found by solving for the smallest value of n that makes this inequality true: Fn >(d._2-d._1)/ε,
 * where n is the Fibonacci number from the sequence {F0,F1, F2, ...}. The Fibonacci search concept involves placing two
 * experiments between [a,b] using the ratios of Fibonacci numbers. (The limit of the ratio of Fibonacci numbers is the
 * golden section 0.618 but the Fibonacci method converges quicker.)
 *
 * @param f function
 * @param d domain
 *
 * @author Jan Paw
 *         Date: 2/22/14
 */
case class Fibonacci(f: Double => Double, d: (Double, Double)) {

  /**
   * Compute minimum with tolerance ε
   *
   * @param ε tolerance
   * @return minimum (x,y)
   */
  def apply(ε: Double): (Double, Double) = {
    val a: Double = d._1
    val b: Double = d._2

    val k = φ((b - a) / ε)
    def β(k: Int): Double = φk(k - 1).toDouble / φk(k).toDouble
    var ai: Double = a
    var bi: Double = b
    var ci: Double = bi - β(k) * (bi - ai)
    var di: Double = ai + bi - ci

    for (i <- 0 to k - 3) {
      if (f(ci) < f(di))
        bi = di
      else
        ai = ci

      ci = bi - β(k - i) * (bi - ai)
      di = ai + bi - ci
    }

    val min = (ai + bi) / 2
    (min, f(min))
  }

  lazy val φk: Stream[Int] = 0 #::
    1 #::
    φk.zip(φk.tail).map {
      k => k._1 + k._2
    }

  @tailrec
  final def findFrom(d: Double, k: Int): Int =
    if (φk(k) < d) findFrom(d, k + 1) else k

  def φ(d: Double): Int = findFrom(d, 0)

  def repeat(body: => Unit) = new {
    def until(condition: => Boolean) = {
      do {
        body
      } while (!condition)
    }
  }
}
