package pp201701.hw2
import pp201701.hw2.Data._
import scala.annotation.tailrec

/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as while, for, return, and so on. You may get deduction
      in your points.
 */

/*
 Implement below functions, which is currently blank. (???)
 */

object Main {
  
  /*
   Exercise 1: IList Map
   Write a map function that applies the given function to all elements of the given IList.
   */
  def map(xs: IList)(f: Int => Int): IList = xs match {
    case INil() => INil()
    case ICons(x, xs) => ICons(f(x), map(xs)(f))
  }

  /*
   Exercise 2: IList Reverse
   Write a reverse function that reverses the order of the given IList.
   */
  def reverse(xs: IList): IList = {
    @tailrec
    def _reverse(xs: IList, accu: IList): IList = xs match {
      case INil() => accu
      case ICons(x, xs) => _reverse(xs, ICons(x, accu))
    }
    _reverse(xs, INil())
  }
  
  /*
   Exercise 3: Exp Calculator
   Given Exp, calculate the result of Int value.
   For each case class Add/Sub/Mul, you may interpret them as
   normal integer operators: +, -, *.
   */
  def calculate(x: Exp): Int = x match {
    case EInt(x) => x
    case EAdd(x, y) => calculate(x) + calculate(y)
    case ESub(x, y) => calculate(x) - calculate(y)
    case EMul(x, y) => calculate(x) * calculate(y)
  }
}
