package lab_12

class Obora(private val wlasciciel: String, private val liczbaBoksow: Int, private val id: Int) {
  private var zwierzaki = new Array[Zwierzak](liczbaBoksow);
  private var liczbaZwierzakow = 0;


  def dodajZwierzaka(zwierzak: Zwierzak): Unit = {
    if (zwierzak.pobierzObore() != null) then {
      println("Zwierzak ma juz swoja obore. Nie przecinamy zwierzat na pol!")
    } else if liczbaZwierzakow < liczbaBoksow then {
      zwierzaki(liczbaZwierzakow) = zwierzak
      zwierzak.przypiszObore(this)
      liczbaZwierzakow = liczbaZwierzakow + 1
    } else
      println("Obora jest przepelniona. Dbamy o dobre samopoczucie zwierzat!")
  }

  def usunZwierzaka(zwierzakDoUsuniecia: Zwierzak): Unit = {
    var pos = 0;
    while (pos < zwierzaki.length) {
      if zwierzaki(pos).equals(zwierzakDoUsuniecia) then {
        zwierzakDoUsuniecia.przypiszObore(null);
        liczbaZwierzakow = liczbaZwierzakow - 1
        zwierzaki(pos) = zwierzaki(liczbaZwierzakow)
      }
      pos = pos + 1;
    }
  }

  def przeniesDoInnejObory(zwierzak: Zwierzak, other: Obora): Unit = {
    if !other.czyPelna() then
      usunZwierzaka(zwierzak)
      other.dodajZwierzaka(zwierzak)
    else
      println("Obora jest przepelniona!")
  }

  def czyPelna(): Boolean = {
    return liczbaZwierzakow == liczbaBoksow
  }

  def wyswietlListe(): Unit = {
    var pos = 0

    printf("\n\n Lista zwierzat w oborze %20s\n", id)
    printf("%10s %10s %10s\n", "Imie", "Gatunek", "Rok ur")
    while (pos < liczbaZwierzakow) {
      val current = zwierzaki(pos)
      printf("%10s %10s %10s \n", current.imie, current.gatunek, current.rok_ur)
      pos = pos + 1
    }
  }
}

object Obora:
  private var id = 0

  def pobierzUnikalneID: Int =
    id = id + 1
    return id

  def apply(wlasciciel: String, liczbaBoksow: Int) =
    new Obora(wlasciciel, liczbaBoksow, pobierzUnikalneID)


@main def test3(args: String*): Unit = {
  val obora_1 = Obora("Jan Majski", 2);
  val obora_2 = Obora("Jan Majski", 2);
  val zwierzak_1 = Zwierzak("pies", "Burek", 2020);
  val zwierzak_2 = Zwierzak("pingwin", "furtek", 1020);
  val zwierzak_3 = Zwierzak("sowa", "lima", 1220);

  obora_1.dodajZwierzaka(zwierzak_1);
  obora_1.dodajZwierzaka(zwierzak_2);
  obora_1.dodajZwierzaka(zwierzak_3);
  obora_1.wyswietlListe();

  obora_2.dodajZwierzaka(zwierzak_1);
  obora_2.dodajZwierzaka(zwierzak_2);
  obora_2.dodajZwierzaka(zwierzak_3);
  obora_2.wyswietlListe();

  obora_1.usunZwierzaka(zwierzak_1)
  obora_2.dodajZwierzaka(zwierzak_1)

  obora_1.wyswietlListe()
  obora_2.wyswietlListe()

  obora_2.przeniesDoInnejObory(zwierzak_3, obora_1)

  obora_1.wyswietlListe()
  obora_2.wyswietlListe()
}
