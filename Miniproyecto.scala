// Metodo Simpmsom 1/3.
def integracion(a: Int, b : Int,f : Double => Double) = {
val intermedio = ((a+b) / 2.0)
val fa = f(a);
val fi = f(intermedio)
val fb = f(b)
(b-a) * (fa + 4 * fi + fb) / 6}

// Metodo Simpsom 1/3 compuesto

 def integracioncomp(a: Int, n: Int, f: Double => Double):Double = {
    val intermedio = (J:Double)=> f(xJ(2*j-2)) +4*f(xJ(2*j-1))+f(xj(2*j))
    val h = (b-a)/n
    val xJ = (J:Double)=>a+(j*h)
    (1 to 2).map/(intermedio(_))(h/3)
    }


  