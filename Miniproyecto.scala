// Metodo Simpmsom 1/3.
/*def integracion(a: Int, b : Int,f : Double => Double) = {
val intermedio = ((a+b) / 2.0)
val fa = f(a);
val fi = f(intermedio)
val fb = f(b)
(b-a) * (fa + 4 * fi + fb) / 6
}
*/

//Metodo Simpsom 1/3

def integracion(a:Int, b: Int, f:Double=>Double):Double={
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}

// Metodo Simpsom 1/3 compuesto

 def integracioncomp(a: Double, b: Double, c: Int, f: Double => Double):Double = {
    val intermedio = (j:Double)=> f(xj(2*j-2)) +4*f(xj(2*j-1))+f(xj(2*j))
    val h = (b-a)/c
    val xj = (j:Double)=>a+(j*h)
    (h/3) * (1 to (n/2)).map(intermedio(_)).sum
    }

// Metodo Simpsom 1/3 Extendida.

def integracionEx(a:Double, b:Double, f:Double => Double):Double = {
    val c = 2* (b-a)
    val h = (b-a)/c
    val intgr = (i:double) => f(a+(j*h))
    val intgr2 = (j:Double) => f(a+(j*h))
    (h/3) * (f(a)+(4*Range(1, c.toInt, 2).map(intgr(_)).sum) + (2*Range(2,(c-1).toInt, 2).map(intgr2(_)).sum)+f(b))


}

// Funcion Error

def FuncionERR(a : Double, b : Double) = (Math.abs(a-b))


val fn = 8
val z = (a : Double) => -Math.pow(a,2) + (8*a) -12
integracion(3, 5, z)
integracioncomp(3, 5, fn. z)
integracionEx(3, 5, z)
FuncionERR(7.33,integracioncomp(3,5,fn.z))
FuncionERR(7.33,integracionEx(3,5,z))


val v = (b: Double)=>3*Math.pow(b,2)
integracion(0,2,v)
integracioncomp(0,2,fn,v)
integracionEx(0,2,v)
FuncionERR(8,integracioncomp(0, 2, fn, v))
FuncionERR(8,integracionEx(0, 2, v))

val k = (c:Double)=>c+2*Math.pow(c,2)- Math.pow(c,3)+ 5*Math.pow(c,4)
integracion(-1, 1, k)
integracioncomp(-1, 1, fn, k)
integracionEx(-1, 1, k)
FuncionERR(3.333, integracioncomp(-1, 1, fn, k))
FuncionERR(3.333, integracionEx(-1, 1, k))


val l = (d: Double)=>((2*x+1)/Math.pow(d,2)+d)
integracion(1, 2, l)
integracioncomp(1, 2, fn, l)
integracionEx(1, 2, l)
FuncionERR(1.09861, integracioncomp(1, 2, fn, l))
FuncionERR(1.09861, integracionEx(1, 2, l))

val ñ = (e:Double)=>Math.pow(Marh.E, e)
integracion(0, 1, ñ)
integracioncomp(0, 1, fn, ñ)
integracionEx(0, 1, ñ)
FuncionERR(1.71828, integracioncomp(0, 1, fn , ñ))
FuncionERR(1.71828, integracionEx(0, 1, ñ))

val p = (f: Double)=>(1/Math.sqrt(f-1))
integracion(2, 3, p)
integracioncomp(2, 3, fn, p)
integracionEx(2, 3, p)
FuncionERR(0.828427, integracioncomp(2, 3, fn, p))
FuncionERR(0.828427, integracionEx(2, 3, p))


val i = (g:Double)=>(1/1+Math.pow(g,2))
integracion(0, 1, i)
integracioncomp(0, 1, fn , i)
integracionEx(0, 1, i)
FuncionERR(1.33333, integracioncomp(0, 1, fn , i))
FuncionERR(0.785398, integracionEx(0, 1, i))
























  