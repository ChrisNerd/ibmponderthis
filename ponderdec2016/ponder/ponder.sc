object ponder {

def dotProduct(as: Iterable[Int], bs: Iterable[Char]) = {
 (for ((a, b) <- as zip bs) yield (
    (a,b) match {
   case (_,'0') => 0
   case _   => a
   })) sum
 }                                                //> dotProduct: (as: Iterable[Int], bs: Iterable[Char])Int
val n=4                                           //> n  : Int = 4
val x = (1 to (scala.math.pow(2.0,n)-1).toInt) map (_.toBinaryString.toCharArray)//.reverse.padTo(n,"0").reverse.toString)
                                                  //> x  : scala.collection.immutable.IndexedSeq[Array[Char]] = Vector(Array(1), A
                                                  //| rray(1, 0), Array(1, 1), Array(1, 0, 0), Array(1, 0, 1), Array(1, 1, 0), Arr
                                                  //| ay(1, 1, 1), Array(1, 0, 0, 0), Array(1, 0, 0, 1), Array(1, 0, 1, 0), Array(
                                                  //| 1, 0, 1, 1), Array(1, 1, 0, 0), Array(1, 1, 0, 1), Array(1, 1, 1, 0), Array(
                                                  //| 1, 1, 1, 1))

/*val C = Vector(
Vector(1,-1,3,5),
Vector(2,-2,6,10),
Vector(3,-3,7,11),
Vector(4,-4,4,12)
) */                                              //> C  : scala.collection.immutable.Vector[scala.collection.immutable.Vector[Int
                                                  //| ]] = Vector(Vector(1, -1, 3, 5), Vector(2, -2, 6, 10), Vector(3, -3, 7, 11),
                                                  //|  Vector(4, -4, 4, 12))

x                                                 //> res0: scala.collection.immutable.IndexedSeq[Array[Char]] = Vector(Array(1), 
                                                  //| Array(1, 0), Array(1, 1), Array(1, 0, 0), Array(1, 0, 1), Array(1, 1, 0), Ar
                                                  //| ray(1, 1, 1), Array(1, 0, 0, 0), Array(1, 0, 0, 1), Array(1, 0, 1, 0), Array
                                                  //| (1, 0, 1, 1), Array(1, 1, 0, 0), Array(1, 1, 0, 1), Array(1, 1, 1, 0), Array
                                                  //| (1, 1, 1, 1))
 /*                                               //> Cx  : Int = 3
 val Cx= dotProduct(C(0), x(6))
 val Cx2= dotProduct(C(1), x(6))                  //> Cx2  : Int = 6
 val Cx3= dotProduct(C(2), x(6))                  //> Cx3  : Int = 7
 val Cx4= dotProduct(C(3), x(6))                  //> Cx4  : Int = 4
*/

val C = Vector(
Vector(
1,1,1,1,1,1,1,1,1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,
0,0,0,0,0,0,0,0,0),
Vector(
1,1,1,-1,-1,-1,0,0,0,
1,1,1,-1,-1,-1,0,0,0,
1,1,1,-1,-1,-1,0,0,0),
Vector(
1,1,1,1,1,1,1,1,1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,
0,0,0,0,0,0,0,0,0),
Vector(
1,0,-1,1,1,1,1,1,1,
-1,-1,-1,-1,-1,-1,-1,-1,-1,
0,0,0,0,0,0,0,0,0)
)




}