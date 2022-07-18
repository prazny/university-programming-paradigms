package lab_12.ptaki

sealed trait Doswiadczenie

case object Brak extends Doswiadczenie

case object Slabo extends Doswiadczenie

case object Dobrze extends Doswiadczenie

case object Znakomicie extends Doswiadczenie

case object Swietnie extends Doswiadczenie


sealed trait Cecha

case object Latam extends Cecha

case object Biegam extends Cecha

case object Plywam extends Cecha

case object Nurkuje extends Cecha

abstract class Ptak {
  println("Pochodzę od dinozaurów!")
  val cechy: List[(Doswiadczenie, Cecha)]

  def czyPosiadaCeche(szukanaCecha: Cecha): Boolean = {
    for (cecha <- cechy) {
      if cecha._2 == szukanaCecha then
        return true
    }
    return false;
  }

  def pochwalSie(szukanaCecha: Cecha): String = {
    for (cecha <- cechy) {
      if cecha._2 == szukanaCecha then
        if cecha._1 == Brak then
          return (cecha._2.toString)
        else
          return (cecha._1.toString) + " " + (cecha._2.toString)
    }
    return ""
  }

}

class Kura extends Ptak /*with Latam("Slabo") with Biegam("Dobrze")*/ {
  val cechy: List[(Doswiadczenie, Cecha)] = List((Slabo, Latam), (Dobrze, Biegam))

  override def toString: String = "Kura"
}

class Sokol extends Ptak /*with Latam("Znakomicie")*/ {
  val cechy: List[(Doswiadczenie, Cecha)] = List((Znakomicie, Latam))

  override def toString: String = "Sokol"
}

class Strus extends Ptak /*with Biegam("Znakomicie")*/ {
  val cechy: List[(Doswiadczenie, Cecha)] = List((Swietnie, Biegam))

  override def toString: String = "Strus"
}

class Golab extends Ptak /*with Latam("Dobrze") with Plywam with Biegam("Słabo")*/ {
  val cechy: List[(Doswiadczenie, Cecha)] = List((Dobrze, Latam), (Brak, Plywam), (Slabo, Biegam))

  override def toString: String = "Golab"
}


class Pingwin extends Ptak /*with Nurkuje  with Plywam*/ {
  val cechy: List[(Doswiadczenie, Cecha)] = List((Brak, Nurkuje), (Brak, Plywam))

  override def toString: String = "Pingwin"
}
