def pred1(x: Int) = x > 5
def pred2(x: Int) = (x < 60)

def zad3(list: List[Int], p1: (Int => Boolean), p2: (Int => Boolean)): Boolean =
  def zad3WithMin(list: List[Int], helper: List[Int], p1: (Int => Boolean), p2: (Int => Boolean), min: Int): Boolean =
    helper match
      case (h :: t) => if p1(h) then zad3WithMin(list, t, p1, p2, min) else false
      case Nil => p2(list.head)

  def findMin(list: List[Int], min: Int): Int =
    list match
      case h :: t => if min > h then findMin(t, h) else findMin(t, min)
      case Nil => min

  zad3WithMin(list, list, p1, p2, findMin(list.tail, list.head))

zad3(List(10, 6, 9, 77, 66), pred1, pred2)