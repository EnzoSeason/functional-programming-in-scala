package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal(b() * b() - 4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal{
      val root1 = (-b() + math.sqrt(delta())) / (2 * a())
      val root2 = (-b() - math.sqrt(delta())) / (2 * a())
      Set(root1, root2)
    }
