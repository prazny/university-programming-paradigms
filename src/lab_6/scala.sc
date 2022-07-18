sealed trait Plec
  case object Kobieta extends Plec
  case object Mezczyzna extends Plec

sealed trait Osoba
  case class osoba(imie: String, nazwisko: String) extends Osoba


def podaj_plec(os: osoba):Plec =
  if os.imie.toLowerCase().last == 'a' then Kobieta
  else Mezczyzna


val osoba1 = osoba("Alicja", "Xyz")
val osoba2 = osoba("Mariusz", "Abc")
val osoba3 = osoba("Daria", "Uio")
val osoba4 = osoba("Patryk", "Qwe")

osoba1.imie.last

podaj_plec(osoba1)
podaj_plec(osoba2)
podaj_plec(osoba3)
podaj_plec(osoba4)

