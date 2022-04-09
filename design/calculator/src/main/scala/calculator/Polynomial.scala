package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal{
      val aVal = a()
      val bVal = b()
      val cVal = c()
      bVal * bVal - 4 * aVal * cVal
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal{
      val dVal = delta()

      if (dVal < 0) Set()
      else {
        val aVal = a()
        val bVal = b()
        val root1 = (-bVal + math.sqrt(dVal)) / (2 * aVal)
        val root2 = (-bVal - math.sqrt(dVal)) / (2 * aVal)
        Set(root1, root2)
      }
    }
