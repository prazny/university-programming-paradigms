def f(x: Double) = Math.cos(x)
def g(x: Double) = Math.sin(x)

val fPrim = (f: (Double => Double), dx: Double) => (x: Double) => ((f(x + dx) - f(x)) / dx)
val fPrimSpec = (f: (Double => Double)) => (x: Double) => ((f(x + 0.0000001) - f(x)) / 0.0000001)


val zad4 = (firstFunc: (Double => Double), secFunc: (Double => Double)) => (x: Double) =>
  (fPrimSpec(firstFunc) andThen fPrimSpec(secFunc)) (x)


val a = zad4(f, g)
a(0)
a(16)

