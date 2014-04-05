package opt

/**
 * @author Jan Paw 
 *         Date: 3/31/2014
 */
abstract class Optimum(val inf: Double)

case object MIN extends Optimum(Double.PositiveInfinity)

case object MAX extends Optimum(Double.NegativeInfinity)
