package lab_12

class Zwierzak(val gatunek: String, val imie: String, val rok_ur: Int) {
  private var obora:Obora = null;

  def przypiszObore(newObora: Obora):Unit =
    obora = newObora;

  def pobierzObore():Obora =
    obora;
}
