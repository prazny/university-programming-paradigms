import scala.annotation.tailrec

def zad1(dokladnosc: Double): Double =
  val pen_tmp = Math.sqrt(0.5)
  val last_tmp = pen_tmp * Math.sqrt(0.5 + (0.5 * pen_tmp))

  @tailrec
  def zad1_tail(dokladnosc: Double, pen: Double, last: Double): Double =
    val x = last * Math.sqrt(0.5 + (0.5 * pen_tmp))
    if Math.abs(x - pen) < dokladnosc then 2 / last
    else zad1_tail(dokladnosc, last, x)

  zad1_tail(dokladnosc, pen_tmp, last_tmp)

zad1(1)

val a = Math.sqrt(0.5)
val b = a * Math.sqrt(0.5 + 0.5 * a)
val c = b * Math.sqrt(0.5 + 0.5 * b)
val d = c * Math.sqrt(0.5 + 0.5 * c)
val e = d * Math.sqrt(0.5 + 0.5 * d)
val f = e * Math.sqrt(0.5 + 0.5 * e)

2 / a
2 / b
2 / c
2 / d
2 / e

Math.sqrt(0.5)