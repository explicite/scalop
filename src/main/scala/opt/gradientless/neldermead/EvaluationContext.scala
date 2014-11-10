package opt.gradientless.neldermead

import util.Cache

case class EvaluationContext(private val fun: (Seq[Double]) => Double,
                             α: Double = 1.0,
                             β: Double = 0.5,
                             γ: Double = 2.0,
                             δ: Double = 0.5) {

  private val cached =  Cache(fun)
  def function = cached

}