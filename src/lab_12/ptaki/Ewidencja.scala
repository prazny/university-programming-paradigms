package lab_12.ptaki

import lab_12.ptaki.Ptak

class Ewidencja {
  var listaPtakow: List[Ptak] = Nil;
  var liczbaPtakow = 0

  def dodajPtaka(ptak: Ptak): Unit = {
    listaPtakow = ptak :: listaPtakow
    liczbaPtakow = liczbaPtakow + 1
  }

  def listaPtakow(cecha: Cecha): Unit = {
    var counter = 1
    for (ptak <- listaPtakow)
      if ptak.czyPosiadaCeche(cecha) then
        printf("Ptak nr %2d - %10s - %20s\n", counter + 1, ptak.toString, ptak.pochwalSie(cecha))
      counter = counter + 1
  }

}

@main def test3(args: String*): Unit = {
  val ewidencja = new Ewidencja
  ewidencja.dodajPtaka(new Pingwin)
  ewidencja.dodajPtaka(new Golab)
  ewidencja.dodajPtaka(new Strus)
  ewidencja.dodajPtaka(new Sokol)
  ewidencja.dodajPtaka(new Kura)
  ewidencja.dodajPtaka(new Pingwin)
  ewidencja.dodajPtaka(new Golab)
  ewidencja.dodajPtaka(new Strus)

  println()
  ewidencja.listaPtakow(Plywam)
  println()
  ewidencja.listaPtakow(Latam)
}