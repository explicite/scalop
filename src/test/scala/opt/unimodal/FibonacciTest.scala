package opt.unimodal

import org.scalatest.{ShouldMatchers, FunSuite}
import math.{sin, cos, tan, Pi}

/**
 * @author Jan Paw
 *         Date: 2/22/14
 */
class FibonacciTest extends FunSuite with ShouldMatchers {
  val ε = 0.000001

  test("Fibonacci: min sin(x) over [π,2π]") {
    val fibonacci = new Fibonacci(sin, (Pi, 2 * Pi))
    val min = 3.0 * Pi / 2.0
    fibonacci(ε)._1 should equal(min +- ε)
  }

  test("Fibonacci: min cos(x) over [π,2π]") {
    val fibonacci = new Fibonacci(cos, (Pi, 2 * Pi))
    val min = Pi
    fibonacci(ε)._1 should equal(min +- ε)
  }

  test("Fibonacci: min tan(x) over [π,2π]") {
    val fibonacci = new Fibonacci(tan, (Pi / 2, 3 / 2 * Pi))
    val min = Pi / 2
    fibonacci(ε)._1 should equal(min +- ε)
  }
}
