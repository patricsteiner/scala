package pp201701.hw2.Test
import pp201701.hw2.Main._
import pp201701.hw2.Data._
import pp201701.hw2.Data.INil
import pp201701.hw2.Data.IList
import pp201701.hw2.Data.ICons
import pp201701.hw2.Data.EInt
import pp201701.hw2.Data.EAdd

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  def listIntToIList(xs: List[Int]): IList =
    xs match {
      case (h :: t) => ICons(h, listIntToIList(t))
      case Nil => INil()
    }
  // Problem 1
  {
    val a = listIntToIList(List(1, 2, 3, 4))
    val b = listIntToIList(List(2, 4, 6, 8))
    print_result(map(a)(_ * 2) == b)
  }
  // Problem 2
  {
    val a = listIntToIList(List(1, 2, 3, 4))
    val b = listIntToIList(List(4, 3, 2, 1))
    print_result(reverse(a) == b)
  }

  // Problem 3
  {
    val three = EInt(3)
    val two = EInt(2)
    print_result(calculate(EAdd(two, three)) == 5)
  }
}
