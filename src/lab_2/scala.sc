

def zad1a[A](a: A, b: Int, c: Float): (List[A], List[Int]) =
{
  if b > 6 && c > 6 then (List(a), List(b))
  else (List(a, a), List(b, b))
}

var checkA = zad1a(4, 12, 12.0)

def zad1b(a: List[Float], b: List[List[Float]]): Boolean =
{
  if a.head + (b.flatten).head > 10 then true
  else false
}


var checkB = zad1b(List(2, 2, 6), List(List(2), List(2, 6)))

def zad1c[A](a: List[A], b: List[A]):Int =
{
  if a.head == b.head then a.length
  else b.length
}

var checkC = zad1c(List(1,6,3), List(7,4))

def divideListHelper(xs: List[Int], n:Int, a:List[Int], b:List[Int]):(List[Int], List[Int])  =
  if xs == Nil then (a, b)
  else if xs.head <= n then divideListHelper(xs.tail, n, a ::: List(xs.head), b)
  else divideListHelper(xs.tail, n, a, b ::: List(xs.head))

def divideLists(xs: List[Int], n: Int):(List[Int], List[Int]) =
  divideListHelper(xs, n, List(), List())

var check = divideLists(List(5, 12, 1, 2, 65, 32), 0)