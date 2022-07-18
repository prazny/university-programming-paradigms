package lab_13

class Patients(val buf: Buffer, val count: Int) extends Thread :
  override def run: Unit = {
    var counter = 0
    while (counter < count)
      counter = counter + 1
      buf.put(counter)

  }
end Patients

class Doctor(val buf: Buffer) extends Thread :
  override def run(): Unit = {
    while (1 == 1)
      buf.take()
  }
end Doctor

class Buffer():
  private var isRoomEmpty = true
  private var patientID = 0

  def take(): Unit = this.synchronized {
    while isRoomEmpty do wait()
    isRoomEmpty = true
    println(s"Z gabinetu wychodzi pacjent $patientID")
    notifyAll()
  }

  def put(patient_id: Int): Unit = this.synchronized {
    while !isRoomEmpty do wait()
    isRoomEmpty = false
    patientID = patient_id
    println(s"Do gabinetu wchodzi pacjent $patientID")
    notifyAll()

  }
end Buffer

object Enviorment:
  def main(args: Array[String]): Unit =
    val buf = new Buffer()
    new Doctor(buf).start()
    new Patients(buf, 10).start()

