package pp201701.hw5
import pp201701.hw5.Data.DataBundle._

object Main extends App {
  /*
   Implement traits for complex number addition/multiplication.
   */
  trait ComplexNumberAdd extends ComplexNumberSpec {
    def add(that: ComplexNumberSpec): ComplexNumberImpl = {
      new ComplexNumberImpl(true, real + that.real, imaginary + that.imaginary)
    }
  }

  trait ComplexNumberMult extends ComplexNumberSpec {
    def mult(that: ComplexNumberSpec): ComplexNumberImpl = {
      new ComplexNumberImpl(true, real * that.real - imaginary * that.imaginary, real * that.imaginary + that.real * imaginary)
    }
  }

  /*
   Spec of eval
   Input: A polynomial, represented in a list of coefficients.
          For example, (1, 0, i, -1) = 1 + (0 * x) + (i * x^2) + (-1 * x^3)
          Nil means 0
   Output: Calculated result of given polynomial with "this" complex number.
          For example, if (1 + 0*i) calls "eval(1, 0, i, -1)", then output is:
          1 + (0 * (1 + 0*i)) + (i * (1 + 0*i)^2) + (-1 * (1 + 0*i)^3)
          = 1 + 0 + i - 1
          = 0 + i
   */
  trait ComplexNumberEval
      extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult {
    def eval(coeffs: List[ComplexNumberEval]): ComplexNumberSpec = {
      def _pow(exponent: Int): ComplexNumberSpec = {
        if (exponent == 0) new ComplexNumberImpl(true, 1, 0)
        else if (exponent == 1) this
        else mult(_pow(exponent-1))
      }
      def _eval(exponent: Int, coeffs: List[ComplexNumberEval], accu: ComplexNumberEval)
        : ComplexNumberSpec = coeffs.headOption match {
        case Some(head) => _eval(exponent+1, coeffs.tail, accu.add(head.mult(_pow(exponent))))
        case None => accu
      }
      _eval(0, coeffs, new ComplexNumberImpl(true, 0, 0))
    }
  }

  /*
   Implement each missing fields so that it compiles.
   For constructor,
     if "isRectangular" is true,
       use "arg1" as "real" and "arg2" as "imaginary" in rectangular representation.
     else,
       use "arg1" as "magnitude" and "arg2" as "angle" in polar representation.

   * Note: The "angle" value should always be normalized.

   */
  class ComplexNumberImpl (isRectangular: Boolean, arg1: Double, arg2: Double)
      extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult
      with ComplexNumberEval {

    val real: Double = if (isRectangular) arg1 else ComplexNumberSpec.polarToRectangular(arg1, ComplexNumberSpec.normalizeAngle(arg2))._1
    val imaginary: Double = if (isRectangular) arg2 else ComplexNumberSpec.polarToRectangular(arg1, ComplexNumberSpec.normalizeAngle(arg2))._2
    val magnitude: Double = if (!isRectangular) arg1 else ComplexNumberSpec.rectangularToPolar(arg1, arg2)._1
    val angle: Double = if (!isRectangular) arg2 else ComplexNumberSpec.rectangularToPolar(arg1, arg2)._2

    def makeRectangular(real: Double, imaginary: Double): ComplexNumberSpec = {
      new ComplexNumberImpl(true, real, imaginary)
    }
    def makePolar(magnitude: Double, angle: Double): ComplexNumberSpec = {
      new ComplexNumberImpl(false, magnitude, angle)
    }
  }

}
