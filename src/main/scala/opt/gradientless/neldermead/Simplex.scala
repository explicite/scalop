package opt.gradientless.neldermead

case class Simplex(points: Seq[Seq[Double]])(implicit context: EvaluationContext) {

  import context._

  val values = points.par.map(function).seq
  val min = points(values.indexOf(values.min))
  val max = points(values.indexOf(values.max))

  def transform = Simplex {
    val cog = points.filter(_ != max).foldLeft(Seq.fill(points.head.size)(0d))((re, ce) => re.zip(ce) map { case (l, r) => l + r}) map (_ / (points.size - 1))
    val reflection = cog.zip(cog.zip(max) map { case (l, r) => (l - r) * α}) map { case (l, r) => l + r}
    if (function(min) <= function(reflection) && function(reflection) < function(max)) {
      points map { point => if (point == max) reflection else point}
    } else if (function(reflection) < function(min)) {
      val expansion = cog.zip(reflection.zip(cog) map { case (l, r) => (l - r) * γ}) map { case (l, r) => l + r}
      if (function(expansion) < function(reflection))
        points map { point => if (point == max) expansion else point}
      else
        points map { point => if (point == max) reflection else point}
    } else {
      val narrow = cog.zip(max.zip(cog) map { case (l, r) => (l - r) * β}) map { case (l, r) => l + r}
      if (function(narrow) < function(max))
        points map { point => if (point == max) narrow else point}
      else
        points map { point => if (point != min) point.zip(min) map { case (r, l) => (r + l) * δ} else point}
    }
  }

  def transformable(ε: Double): Boolean = !points.filter(_ != min).forall(point => scala.math.sqrt(point.zip(min).map { case (l, r) => (l - r) * (l - r)}.sum) < ε)

}
