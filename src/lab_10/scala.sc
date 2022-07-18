val arr = Array(
  Array(1, 2, 3, 4, 5),
  Array(2, 2, 3, 4, 5),
  Array(3, 2, 3, 4, 5),
  Array(4, 2, 3, 4, 5),
  Array(5, 2, 3, 4, 5),
)


def sumRow(arr: Array[Array[Int]]):Array[Int] =
  def sumRows_tail(arr: Array[Array[Int]], sum_arr: Array[Int], counter: Int):Array[Int] =
    if counter >= arr.length then sum_arr
    else sumRows_tail(arr, sum_arr :+ arr(counter).foldLeft(0)((a, b) => a + b ), counter+1)
  sumRows_tail(arr, Array(), 0)

sumRow(arr)


def sumRows_imp(arr: Array[Array[Int]]):Array[Int] =
  val sum_arr = Array.fill(arr.length){0}
  for (i <- 0 to arr.length-1) {
    for(j<-0 to arr(i).length-1) {
      sum_arr(i) = sum_arr(i) + arr(i)(j)
    }
  }
  sum_arr


sumRows_imp(arr);;
