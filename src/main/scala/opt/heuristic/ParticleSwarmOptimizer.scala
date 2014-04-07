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
   * Find minimum
   *
   * @param a number of search actors
   * @param i number of iterations
   * @param vMax
   * @param wMin min of inertia weight
   * @param wMax max of inertia weight
   * @param ilr individual learning rate
   * @param sp social parameter
   *
   *
   * @return minimum (best position)
   */
  def min(a: Int, i: Int, vMax: Double, wMin: Double = 0.4, wMax: Double = 0.9, ilr: Double = 2d, sp: Double = 2d): Seq[Double] = optimum(a, i, vMax, wMin, wMax, ilr, sp)(MIN)

  /**
   * Find maximum
   *
   * @param a number of search actors
   * @param i number of iterations
   * @param vMax
   * @param wMin min of inertia weight
   * @param wMax max of inertia weight
   * @param ilr individual learning rate
   * @param sp social parameter
   *
   * @return minimum (best position)
   */
  def max(a: Int, i: Int, vMax: Double, wMin: Double = 0.4, wMax: Double = 0.9, ilr: Double = 2d, sp: Double = 2d): Seq[Double] = optimum(a, i, vMax, wMin, wMax, ilr, sp)(MAX)

  private def optimum(numberOfActors: Int,
                      iterations: Int,
                      vMax: Double,
                      minInertiaWeight: Double,
                      maxInertiaWeight: Double,
                      individualLearningRate: Double,
                      socialParameter: Double)(opt: Optimum): Seq[Double] = {

    val velocity: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(numberOfActors)(MutableSeq.fill(DIM)(0d))
    val pBestScore: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(numberOfActors)({
      val score = MutableSeq.fill(numberOfActors)(0d)
      score(0) = opt.opposite().inf
      score
    })

    val pBest: MutableSeq[MutableSeq[Double]] = MutableSeq.fill(numberOfActors)(MutableSeq.fill(DIM)(0d))
    var gBestScore: Double = opt.inf
    var gBest: MutableSeq[Double] = MutableSeq.fill(DIM)(0d)

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
        pos(position) = pos(position).zip(b).map(ab => if (ab._1 > ab._2._2 || ab._1 < ab._2._1)  (RAND.nextDouble() * (ab._2._2 - ab._2._1 + 1d)) + ab._2._1  else ab._1)
      }

      for (i <- 0 until pos.length) {
        val fitness = f(pos(i))

        if (pBestScore(i)(0) > fitness) {
          pBestScore(i)(0) = fitness
          pBest(i) = pos(i)
        }

        if (gBestScore > fitness) {
          gBestScore = fitness
          gBest = pos(i)
        }

        val w = maxInertiaWeight - (iteration * ((maxInertiaWeight - minInertiaWeight) / iterations.toDouble))

        for (i <- 0 until pos.length) {
          for (j <- 0 until pos(i).length) {
            velocity(i)(j) = w * velocity(i)(j) + socialParameter * RAND.nextDouble * (pBest(i)(j) - pos(i)(j)) + individualLearningRate * RAND.nextDouble() * (gBest(j) - pos(i)(j))

            if (velocity(i)(j) > vMax)
              velocity(i)(j) = vMax

            if (velocity(i)(j) < -vMax)
              velocity(i)(j) = -vMax

            pos(i)(j) = pos(i)(j) + velocity(i)(j)
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
