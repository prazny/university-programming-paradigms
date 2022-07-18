package lab_11

class Wektor (val v: Array[Int]){
  def +(other: Wektor): Wektor =
    val maxSize = other.v.length.max(v.length)
    val newWektor = new Array[Int](maxSize)

    var counter = 0
    while(counter < maxSize) {
      newWektor(counter) = 0
      if(counter < v.length) newWektor(counter) = newWektor(counter) + v(counter)
      if(counter < other.v.length) newWektor(counter) = newWektor(counter) + other.v(counter)

      counter = counter + 1
    }
    return new Wektor(newWektor)

  def -(other: Wektor): Wektor =
    val maxSize = other.v.length.max(v.length)
    val newWektor = new Array[Int](maxSize)

    var counter = 0
    while(counter < maxSize) {
      newWektor(counter) = 0
      if(counter < v.length) newWektor(counter) = newWektor(counter) + v(counter)
      if(counter < other.v.length) newWektor(counter) = newWektor(counter) - other.v(counter)

      counter = counter + 1
    }
    return new Wektor(newWektor)

  def *+(other: Wektor): Int =
    val maxSize = other.v.length.max(v.length)
    val newWektor = new Array[Int](maxSize)

    var sum = 0;
    var counter = 0
    while(counter < maxSize) {
      var pr = 0

      newWektor(counter) = 0
      if(counter < v.length && counter < other.v.length) sum = sum + (v(counter) * other.v(counter))

      counter = counter + 1
    }
    return sum


}
@main def test1(args: String*): Unit =
  val a = new Wektor(Array(5, 3))
  val b = new Wektor(Array(3, 2))
  val c = new Wektor(Array(3, 2))
  println((a + b) *+ c)


  println((a *+ c))

