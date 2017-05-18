package pp201701.hw5
import pp201701.hw5.Data.DataBundle._

object Main extends App {
  /*
   Implement traits for complex number addition/multiplication.
   */
  trait ComplexNumberAdd extends ComplexNumberSpec {
    def add(that: ComplexNumberSpec): ComplexNumberSpec = {
      makeRectangular(real + that.real, imaginary + that.imaginary)
    }
  }

  trait ComplexNumberMult extends ComplexNumberSpec {
    def mult(that: ComplexNumberSpec): ComplexNumberSpec = {
      makeRectangular(real * that.real - imaginary * that.imaginary, real * that.imaginary + that.real * imaginary)
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
      def _pow(exponent: Int): ComplexNumberEval = {
        if (exponent == 0) makeRectangular(1, 0)
        else if (exponent == 1) this
        else mult(_pow(exponent-1))
      }
      def _eval(exponent: Int, coeffs: List[ComplexNumberEval], accu: ComplexNumberEval)
        : ComplexNumberEval = coeffs.headOption match {
        case Some(head) => _eval(exponent+1, coeffs.tail, accu.add(head.mult(_pow(exponent))))
        case None => accu
      }
      _eval(0, coeffs, makeRectangular(1, 0))
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
        val real =
        val imaginary: Double
        val magnitude: Double
        val angle: Double
        def makeRectangular(real: Double, imaginary: Double): ComplexNumberSpec
        def makePolar(magnitude: Double, angle: Double): ComplexNumberSpec
    if (isRectangular) {
      makeRectangular(arg1, arg2)
    else makePolar(arg1, ComplexNumberSpec.normalizeAngle(arg2))
  }

}
