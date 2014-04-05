package opt.heuristic

import scala.util.Random
import opt.{Optimum, MIN, MAX}
import scala.collection.mutable.{Seq => MutableSeq}

/**
 * The PSO (Particle Swarm Optimizer) is a computational method that optimizes a problem by iteratively trying to
 * improve a candidate solution with regard to a given measure of quality. PSO optimizes a problem by having a
 * population of candidate solutions, here dubbed particles, and moving these particles around in the search-space
 * according to simple mathematical formulae over the particle's position and velocity. Each particle's movement is
 * influenced by its local best known position but, is also guided toward the best known positions in the search-space,
 * which are updated as better positions are found by other particles. This is expected to move the swarm toward the
 * best solutions.
 *
 * @param f function to optimize
 * @param b bounds for variables x1(min, max), x2(min, max) ... xn(min, max)
 *
 * @author Jan Paw
 *         Date: 3/31/2014
 */
case class ParticleSwarmOptimizer(f: (Seq[Double]) => Double, b: Seq[(Double, Double)]) {
  val DIM = b.length
  val RAND = new Random()

  /**
   * TODO
   * @param a
   * @param i
   * @param vMax
   * @param wMin
   * @param wMax
   * @return
   */
  def min(a: Int, i: Int, vMax: Double, wMin: Double, wMax: Double): Seq[Double] = optimum(a, i, vMax, wMin, wMax)(MIN)

  /**
   * TODO
   * @param a
   * @param i
   * @param vMax
   * @param wMin
   * @param wMax
   * @return
   */
  def max(a: Int, i: Int, vMax: Double, wMin: Double, wMax: Double): Seq[Double] = optimum(a, i, vMax, wMin, wMax)(MAX)

  private def optimum(numberOfActors: Int, iterations: Int, vMax: Double, wMin: Double, wMax: Double)(opt: Optimum): Seq[Double] = {
    val vel: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(numberOfActors)(MutableSeq.fill(DIM)(0d))
    val pBestScore: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(DIM)(MutableSeq.fill(DIM)(0d))
    val pBest: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(numberOfActors)(MutableSeq.fill(DIM)(0d))
    var gBestScore: Double = 0d
    var gBest: MutableSeq[Double] = MutableSeq.fill(DIM)(opt.inf)

    val pos: MutableSeq[MutableSeq[Double]] = {
      val positions: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(numberOfActors)(MutableSeq.fill(DIM)(0.0))
      val random: Random = new Random()

      positions.foreach(position => {
        for (dim <- 0 until DIM) {
          val min = b(dim)._1
          val max = b(dim)._2
          position(dim) = (random.nextDouble() * (max - min + 1d)) + min
        }
      })

      positions
    }

    for (iteration <- 0 until iterations) {
      for (position <- 0 until pos.length) {
        pos(position) = backToSpace(pos(position))
      }

      for (i <- 0 until pos.length) {
        val fitness = f(pos(i))

        if (pBestScore(i)(1) > fitness) {
          pBestScore(i)(1) = fitness
          pBest(i) = pos(i)
        }

        if (gBestScore > fitness) {
          gBestScore = fitness
          gBest = pos(i)
        }

        val w = wMax - iteration * ((wMax - wMin) / iterations)

        for (i <- 0 until pos.length) {
          for (j <- 0 until pos(i).length) {
            vel(i)(j) = w * vel(i)(j) + 2d * RAND.nextDouble * (pBest(i)(j) - pos(i)(j)) + 2d * RAND.nextDouble() * (gBest(j) - pos(i)(j))

            if (vel(i)(j) > vMax)
              vel(i)(j) = vMax

            if (vel(i)(j) < -vMax)
              vel(i)(j) = -vMax

            pos(i)(j) = pos(i)(j) + vel(i)(j)
          }
        }
      }
    }

    gBest
  }

  private def backToSpace(position: MutableSeq[Double]): MutableSeq[Double] = {
    def back(ab: (Double, (Double, Double))): Double = {
      val pos = ab._1
      val lb = ab._2._1
      val ub = ab._2._2
      if (pos > ub) {
        ub - Double.MinValue
      } else if (pos < lb) {
        lb + Double.MinValue
      } else pos
    }

    position.zip(b).map(ab => back(ab))
  }
}
