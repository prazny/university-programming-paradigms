import scala.language.postfixOps

val lxs = LazyList(2.0, 2.0, 3.0, 4.0, 5.0)

val lxs: LazyList[Double] = {
  def dodajWielokrotnosci(liczba: Double, wystapienie: Int, docelowo: Int, poprzednia: Double) =
    if wystapienie < docelowo then liczba * poprzednia #:: dodajWielokrotnosci(liczba, wystapienie + 1, docelowo, liczba * poprzednia)
    else helper()

  def helper(x: Double, counter: Int): LazyList[Double] =
    x #:: dodajWielokrotnosci(x, 1, counter, x)

  helper(2, 1)
}

def zad1(lls: LazyList[Double]): LazyList[Double] =
  def dodajWielokrotnosci

  def tail(xs: LazyList[Double]): LazyList[Double] =
    val hd ::# tail = lls
    hs ::# dodajWielokrotnosci(tail)


def zad1(lls: LazyList[Double]): LazyList[Double] =


  val nextNumbers: LazyList[Int] = {
    def loop(v: Int): LazyList[Int] = v #:: loop(v + 1)

    loop(0)
  }

  def pow(x: Double, n: Int): Double =
    n match
      case 0 => 1
      case 1 => x
      case n => pow(x, n - 1) * x

  def lmap_tail(lls: LazyList[Double], n: Int): LazyList[Double] =
    nextNumbers.zip(lls).map(n => pow(n._2, n._1))

  lmap_tail(lls, 0)

zad1(lxs).take(5).toList
lxs
