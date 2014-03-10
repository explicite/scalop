package opt.heuristic

/**
 * The GWO (Grey Wolf Optimizer) algorithm mimics the leadership hierarchy and hunting mechanism of gray wolves in
 * nature proposed by Mirjalili et al. in 2014. Four types of grey wolves such as alpha, beta, delta, and omega are
 * employed for simulating the leadership hierarchy. In addition, three main steps of hunting, searching for prey,
 * encircling prey, and attacking prey, are implemented to perform optimization.
 *
 * @param f function to optimize
 * @param b bounds for variables x1(min, max), x2(min, max) ... xn(min, max)
 *
 * @author Jan Paw
 *         Date: 3/10/14
 */
case class GWO(f: (Seq[Double]) => Double, b: Seq[(Double, Double)]) {

  /**
   * Optimize function
   *
   * @param a number of search actors
   * @param i number of iterations
   *
   * @return optimum (best score, best position)
   */
  def apply(a: Int, i: Int): (Double, Seq[Double]) = ???
}
