// Metodo Simpmsom 1/3.
def integracion(a: Int, b : Int,f : Double => Double) = {
val intermedio = ((a+b) / 2.0)
val fa = f(a);
val fi = f(intermedio)
val fb = f(b)
(b-a) * (fa + 4 * fi + fb) / 6}

// Metodo Simpsom 1/3 compuesto

 def integracioncomp(a: Double, b: Int, c: Double, f: Double => Double):Double = {
    val intermedio = (j:Double)=> f(xj(2*j-2)) +4*f(xj(2*j-1))+f(xj(2*j))
    val h = (b-a)/c
    val xj = (j:Double)=>a+(j*h)
    (h/3) * (1 to (n/2)).map(intermedio(_)).sum
    }

def integracionEx(a:Double, b:Double, f:Double => Double):Double = {
    val c = 2* (b-a)
    val intgr = (j:double) => f(j*h)
}


def iFuncionERR(a : Double, b : Double) = (Math.abs(a-b)
)



  