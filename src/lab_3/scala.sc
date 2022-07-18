import scala.annotation.tailrec
import scala.language.postfixOps

def zad1[A](n: Int, a: Float, eps: Float) =
  @tailrec
  def zad1_iter(n: Int, a: Float, eps: Float, start: Float, end: Float, pen: Float, last: Float): Float =
    val middle = (start + end) / 2
    val val_of_func = a - Math.pow(middle, n)
    val val_of_func_in_start = a - Math.pow(start, n)

    if val_of_func == 0 then middle
    else if (Math.abs(last - pen) < eps) last
    else if val_of_func_in_start * val_of_func < 0 zad1_iter(n, a, eps, start, middle, last, middle)
    else zad1_iter(n, a, eps, middle, end, last, middle)

  if (a == 0 || a == 1) a
  else if a > 1 zad1_iter(n, a, eps, 0, a, -100, -50)
  else zad1_iter(n, a, eps, 0, 1, -100, -50)


zad1(2, 3, 0.00001)
zad1(2, 2, 0.00001)
zad1(3, 2, 0.00001)

def zad2[A](value: A, xs: List[A]): List[Int] = {
  @tailrec
  def find[A](value: A, xs: List[A], list: List[Int], pos: Int): List[Int] = {
    xs match
      case (h :: _) if h == value => find(value, xs.tail, pos :: list, pos + 1)
      case (h :: _) => find(value, xs.tail, list, pos + 1)
      case _ => list match
        case (h :: _) => list
        case _ => throw new Exception("lista nie zawiera elementu")
  }

  find(value, xs, List(), 1)
}
val a = zad2(3, List(5, 2, 3, 5, 3, -2, 3))
val aa = zad2(3, List(3, 3, 3))
val b = zad2(3, List())
val c = zad2(3, List(2, 1, 5))
