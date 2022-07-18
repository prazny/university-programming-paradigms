package lab_13_2

class Buffer:
  private var next: Double = 0
  private var empty = true

  def put(value: Double): Unit = this.synchronized {
    while !empty do
      wait()
    this.next = value
    println(s"Putted: $value")
    empty = false
    notifyAll()
  }

  def take: Double = this.synchronized {
    while empty do wait()
    val toRet = this.next
    empty = true
    notifyAll()
    println(s"taked: $toRet")
    toRet
  }
end Buffer


class ComputeNext(buf: Buffer) extends Thread :
  override def run(): Unit =
    var n = 1
    while (true)
      val doubly: Double = 2 * n
      buf.put((doubly / (doubly - 1)) * (doubly / (doubly + 1)))
      n = n + 1
end ComputeNext

class Result(buf: Buffer, accuracy: Double) extends Thread :
  override def run(): Unit =
    var isFinish: Boolean = false
    var prePrev: Double = buf.take
    var prev: Double = buf.take * prePrev
    var current: Double = buf.take * prev
    while (!isFinish)
      prePrev = prev
      prev = current
      current = buf.take * prev
      if (math.abs(2 * (prePrev - current)) < accuracy)
        println("Wynik: " + prev * 2)
        isFinish = true
end Result

object Enviorment:
  def main(args: Array[String]): Unit =
    val buf = new Buffer
    new ComputeNext(buf).start()
    new Result(buf, 0.00000001).start()