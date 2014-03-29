package opt.heuristic

import scala.collection.mutable.{Seq => MutableSeq}
import scala.util.Random
import scala.math.abs
import opt.heuristic.GWO.{OPT, MIN, MAX}

/**
 * The GWO (Grey Wolf Optimizer) algorithm mimics the leadership hierarchy and hunting mechanism of gray wolves in
 * nature proposed by Mirjalili et al. in 2014. Four types of grey wolves such as alpha, beta, delta, and omega are
 * employed for simulating the leadership hierarchy. In addition, three main steps of hunting, searching for prey,
 * encircling prey, and attacking prey, are implemented to perform optimization.
 *
 * @param f function to optimize
 * @param b bounds for variables x1(min, max), x2(min, max) ... xn(min, max)
 * @param d number of dimensions
 *
 * @author Jan Paw
 *         Date: 3/10/14
 */
case class GWO(f: (Seq[Double]) => Double, b: Seq[(Double, Double)], d: Int) {
  val rand = new Random()

  /**
   * Find minimum
   *
   * @param a number of search actors
   * @param i number of iterations
   *
   * @return minimum (best score, best position)
   */
  def min(a: Int, i: Int): Seq[Double] = optimize(a, i)(MIN)

  /**
   * Find maximum
   *
   * @param a number of search actors
   * @param i number of iterations
   *
   * @return optimum (best score, best position)
   */
  def max(a: Int, i: Int): Seq[Double] = optimize(a, i)(MAX)

  private def optimize(numberOfActors: Int, iterations: Int)(opt: OPT): Seq[Double] = {
    var alphaPos: Seq[Double] = Seq.fill(d)(0.0)
    var alphaScore: Double = opt.inf
    var betaPos: Seq[Double] = Seq.fill(d)(0.0)
    var betaScore: Double = opt.inf
    var deltaPos: Seq[Double] = Seq.fill(d)(0.0)
    var deltaScore: Double = opt.inf

    val positions: MutableSeq[MutableSeq[Double]] = generate

    // Main loop
    for (iteration <- 0 until iterations) {
      for (position <- 0 until positions.length) {
        //Return back the search agents that go beyond the boundaries of the search space
        positions(position) = backToSpace(positions(position))

        // Calculate objective function for each search actors
        val fitness: Double = f(positions(position))

        // Update Alpha, Beta, and Delta
        if (fitness > alphaScore) {
          alphaScore = fitness
          alphaPos = positions(position).clone()
        }

        if (fitness > alphaScore && fitness < betaScore) {
          betaScore = fitness
          betaPos = positions(position).clone()
        }

        if (fitness > alphaScore && fitness > betaScore && fitness < deltaScore) {
          deltaScore = fitness
          deltaPos = positions(position).clone()
        }
      }

      val a = 2 - iteration * (2 / iterations)

      for (i <- 0 until positions.length) {
        for (j <- 0 until positions(i).length) {
          val cAlpha = coefficientVector(a)
          val dAlpha = abs(cAlpha._2 * alphaPos(j) - positions(i)(j))
          val x1 = alphaPos(j) - cAlpha._1 * dAlpha

          val cBeta = coefficientVector(a)
          val dBeta = abs(cBeta._2 * betaPos(j) - positions(i)(j))
          val x2 = betaPos(j) - cBeta._1 * dBeta

          val cDelta = coefficientVector(a)
          val dDelta = abs(cDelta._2 * deltaPos(j) - positions(i)(j))
          val x3 = deltaPos(j) - cDelta._1 * dDelta

          positions(i)(j) = (x1 + x2 + x3) / 3
        }
      }
    }

    alphaPos
  }


  private def coefficientVector(a: Double): (Double, Double) = {
    (2 * a * rand.nextDouble() - 1, 2 * rand.nextDouble())
  }

  private def generate: MutableSeq[MutableSeq[Double]] = {
    val positions: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(d)(MutableSeq.fill(d)(0.0))
    val random: Random = new Random()

    positions.foreach(position => {
      for (dim <- 0 until d) {
        position(dim) = b(dim)._1 + random.nextDouble() * b(dim)._2
      }
    })

    positions
  }

  private def backToSpace(p: MutableSeq[Double]): MutableSeq[Double] = {
    def back(ab: (Double, (Double, Double))): Double = {
      val pos = ab._1
      val lb = ab._2._1
      val ub = ab._2._2
      if (pos > ub) {
        ub
      } else if (pos < lb) {
        lb
      } else pos
    }

    p.zip(b).map(ab => back(ab))
  }
}


object GWO {

  private abstract class OPT(val inf: Double)

  private case object MIN extends OPT(Double.PositiveInfinity)

  private case object MAX extends OPT(Double.NegativeInfinity)

}