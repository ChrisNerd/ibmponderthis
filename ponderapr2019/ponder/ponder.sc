object ponder {
 /* a b c   13  19  37
    d e f   67  7   109
    g h i   43  103 31

 We want all 3 rows, all 3 columns, and both diagonals to average to a prime.
 e.g. (a+b+c)/3 is prime...
 This implies that all 8 sums (a+b+c, d+e+f, ...) must be 0 (mod 3).
 Any integer must be either 0, 1 or 2 (mod 3).
 If an number is 0 (mod 3), it is a multiple of 3. The only prime with this property is 3 itself.
 Since all 9 numbers must be different (otherwise a trivial solution is to make them all the same prime), 3 can be used at most once.
 If we use it once, that implies that there are 2 other pairs of numbers that are (1,2) and (1,2) (mod 3).
 The existance of a single 0 (mod 3) contaminates the whole matrix and there's no way of arrainging 8 other 1's and 2's (mod 3).
 Therefore, candidate solutions are where all 9 numbers are congruant to 1 (mod 3), or all 9 are congruent to 2 (mod 3).
 Note that 2 is not a permissible number either, because the sum of two other (non-2) primes will be even. So the sum
 of all 3 is 0 (mod 2), and therefore not prime.
 
 There's a bunch of different ways you can go about this. I chose by starting with the centre,
 e, being as small as possible,
 since it's involved in the most sums.
 Then choosing a and b arbitrarily.
 Finding the first c, that solves (a+b+c)/3 is prime... likewise...
 Finding the first g. based on c+e+g...
 then i, then d.
 f and h, need to solve 2 sums each at the same time.
  Doing it all 2 (mod 3) yields:
 11 17 23
 47 5  59
 29 71 41
 
 But at least this way avoids needing to solve 3 sums at the same time, and it turns out to be super fast.
 If you do a corner last, then it'll need to solve 3 simulataneously.
 Let's try
 e,a,b,c,d,f,g,h,i
 That has no problem finding the solution
 (13,19,37)
 (31,7,73)
 (43,61,109)
 */
 for (modValue <- (4 to 20).filterNot(_%3 == 0))
 yield {
  val primes1mod3 = Stream.iterate(BigInt(modValue))(_ + 3)
  .filter(_.isProbablePrime(5))
  // primes1mod3.take(5).foreach(println)
  def searchForPrimeSatifying1Equation(excludingList: List[BigInt], x1 : BigInt, x2: BigInt) =
  {
   primes1mod3.filterNot( excludingList.contains )
   .find(x => ((x1+x2+x)/3).isProbablePrime(5)) match{ case Some(x) => x}
  }
  def searchForPrimeSatifying2Equations(excludingList: List[BigInt], x1 : BigInt, x2: BigInt, y1 : BigInt, y2: BigInt) =
  {
   primes1mod3.filterNot( excludingList.contains )
   .find(x => ((x1+x2+x)/3).isProbablePrime(5) && ((y1+y2+x)/3).isProbablePrime(5)) match{ case Some(x) => x}
  }
  def searchForPrimeSatifying3Equations(excludingList: List[BigInt], x1 : BigInt, x2: BigInt, y1 : BigInt, y2: BigInt, z1 : BigInt, z2: BigInt) =
  {
   primes1mod3.filterNot( excludingList.contains )
   .find(x => ((x1+x2+x)/3).isProbablePrime(5) && ((y1+y2+x)/3).isProbablePrime(5) && ((z1+z2+x)/3).isProbablePrime(5)) match{ case Some(x) => x}
  }
  val e = primes1mod3.head
  val a = primes1mod3(1)
  val b = primes1mod3(2)
  val c = searchForPrimeSatifying1Equation(List(a,b,e), a, b)
  val d = primes1mod3(3)
  val f = searchForPrimeSatifying1Equation( List(a,b,c,d,e), d, e)
  val g = searchForPrimeSatifying2Equations(List(a,b,c,d,e,f), a, d, c, e)
  val h = searchForPrimeSatifying1Equation( List(a,b,c,d,e,f,g), b, e)
  val i = searchForPrimeSatifying3Equations(List(a,b,c,d,e,f,g,h), g, h, c, f, a,e)

 /*
 Doing it the simpler way to avoid finding a solution for 3 cases simulatenously.
  val g = searchForPrimeSatifying1Equation(List(a,b,c,e), c, e)
  val i = searchForPrimeSatifying1Equation(List(a,b,c,e,g), a, e)
  val d = searchForPrimeSatifying1Equation(List(a,b,c,e,g,i), a, g)
  val f = searchForPrimeSatifying2Equations(List(a,b,c,d,e,g,i), d, e, c, i)
  val h = searchForPrimeSatifying2Equations(List(a,b,c,d,e,f,g,i), g, i, b, e)
*/

  println("Solution")
  println(a,b,c)
  println(d,e,f)
  println(g,h,i)
  (a,b,c,d,e,f,g,h,i)
 }                                                //> Solution
                                                  //| (13,19,37)
                                                  //| (31,7,73)
                                                  //| (43,61,109)
                                                  //| Solution
                                                  //| (11,17,23)
                                                  //| (23,5,29)
                                                  //| (59,47,71)
                                                  //| Solution
                                                  //| (13,19,37)
                                                  //| (31,7,73)
                                                  //| (43,61,109)
                                                  //| Solution
                                                  //| (17,23,29)
                                                  //| (29,11,47)
                                                  //| (83,53,101)
                                                  //| Solution
                                                  //| (19,31,37)
                                                  //| (37,13,43)
                                                  //| (73,67,61)
                                                  //| Solution
                                                  //| (17,23,29)
                                                  //| (29,11,47)
                                                  //| (83,53,101)
                                                  //| Solution
                                                  //| (19,31,37)
                                                  //| (37,13,43)
                                                  //| (73,67,61)
                                                  //| Solution
                                                  //| (23,29,41)
                                                  //| (41,17,53)
                                                  //| (263,47,83)
                                                  //| Solution
                                                  //| (31,37,43)
                                                  //| (43,19,61)
                                                  //| (67,73,79)
                                                  //| Solution
                                                  //| (23,29,41)
                                                  //| (41,17,53)
                                                  //| (263,47,83)
                                                  //| Solution
                                                  //| (31,37,43)
                                                  //| (43,19,61)
                                                  //| (67,73,79)
                                                  //| Solution
                                                  //| (29,41,53)
                                                  //| (47,23,59)
                                                  //| (83,113,71)
                                                  //| res0: scala.collection.immutable.IndexedSeq[(scala.math.BigInt, scala.math.
                                                  //| BigInt, scala.math.BigInt, scala.math.BigInt, scala.math.BigInt, scala.math
                                                  //| .BigInt, scala.math.BigInt, scala.math.BigInt, scala.math.BigInt)] = Vector
                                                  //| ((13
                                                  //| Output exceeds cutoff limit.
}