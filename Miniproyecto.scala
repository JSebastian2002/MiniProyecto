// Metodo Simpmsom 1/3.

def integracion(a: Int, b : Int,f : Double => Double) = {
val intermedio = ((a+b) / 2.0)
val fa = f(a);
val fi = f(intermedio)
val fb = f(b)
(b-a) * (fa + 4 * fi + fb) / 6}


  