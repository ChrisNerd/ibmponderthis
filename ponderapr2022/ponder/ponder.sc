import scala.annotation.tailrec

object ponder {
def main(args: Array[String]) =
 {
 /*
 For rational entries of x
 let x be a 5-length.
 x1=a
 x2=b
 x3=c
 x4=d
 x5=e
 
 If each entry is of the form p/q and the sum of their squares = 1 then we'll have
 p1^2 / q1^2 + p2^2/q2^ .... = 1.
 multiplying through by the least common multiple of q1^2, q2^2.... we get
 p1^2 * m1^2 + p2^2 * m2^2 ... = m6^2, where m1, m2 ... are integers.
 Each of the 5 terms on the left can be factored
 p1^2 * m1^2 = (p1 * m1)^2  so we'll end up with
 n1^2 + n2^2+ n3^2+n4^2+n5^2 = m6^2. So really we're just want a pythogorean sextet.
 
 We also want them to sum to 0.
 
 Now we want to select any y vector that will dot product with it to 0.
 
 Now that we have y, we can calculate f, and see if we can get a 4.
 
 
 Maybe we should start with the behaviour of f.
 We can make a 5x5 distance matrix d from x.
   x1 x2 x3 x4 x5
 x1 0
 x2    0   dx
 x3       0
 x4  dx      0
 x5  transpose  0
 
 We are looking for "similar" one for y. We're doing a standard sum of squares of their differences.
 So if we calculate the d matrix for each x.... The differences between the dx and dy upper triangles (squared) must be 2 (or less).
 
 Now what about x concatonate x. Like, doubling x in length. Making the d matrix 4 times larger
 
   x1 x2 x3 x4 x5 x1 x2 x3 x4 x5
 x1 0              0
 x2    0   dx         0
 x3       0              0
 x4  dx      0              0
 x5  transpose  0              0
 x1 0              0
 x2    0   dx         0
 x3       0              0
 x4  dx      0              0
 x5  transpose  0              0
 
 Before normalizing each row will have magnitude sqrt(2). (The sum of the first 5 squared =1, the sum of the second 5 =1
 the sum of all 10 squared =2. Take the square root to pathagorize. So to normalize we need to divide each term by the sqrt(2).
 
 That will lead to the sum of all 10 squared = 1.
 If, instead we put 4 copies of x back to back, we'll end up with a 20x20 matrix. Normalizing will need to divide each term by sqrt(4)
 leaving them rational!
 the dx triangle will linearly decrease by 1/sqrt(n), but there are
 |a*xi - a*xj| / |xi-xj| = a.
 and also let (|xi - xj| - |yi - yj|) = deltadx
 (|axi - axj| - |ayi - ayj|) = a * deltadx
 (a|xi - xj| - a|yi - yj|)^2 =
 a^2*(|xi - xj| - |yi - yj|)^2 =
 a^2 * deltadx^2.
 So when we double the length of x, a=1/sqrt(2) but there will be 4 times as many dx triangles.
 ahhh! And the dy will also go down by the same amount. so the dx-dy
 hmmm... maybe not. Was hoping if we can get one deltadx^2 to be = 4, then we'd get any doublings for free and they'd still =4.
 
 A bonus "**" will be given for finding an exact rational solution for any n> 40 ,
 
 Wait a second. We can do that with the trivial solution
 x= 3/5, 4/5
 y = -4/5, 3/5.
 And just putting 16 copies back to back.
 What's the f with that?
   x1 x2
x1  0 1/5
x2     0

   y1 y2
y1  0 7/5 <-- eee!!!
y2     0
 
delta =  7/5
f(x,y) = 49/25 + 49/25 = 98/25 = 3.82

But where does the 7/5 come from?
a/c b/c we get (a+b)/c. and it looks like the f is just 2*(a+b)^2/c^2.
which is = 2* (a^2 + b^2 + 2ab)/c^2 = 2* (c^2 + 2ab)/c^2 = 2 + 4*ab/c^2
hmmm, can that ever = 4? Let's see

2 + 4*ab/c^2 = 4
4*ab/c^2 = 2
2*ab/c^2 = 1
2*ab=c^2.  Don't think so. c's always odd. Isn't c=1 (mod 4)?
I think this is the form of the proof of sqrt(2) is irrational?

 What am I thinking anyway? There's no solution for n=2. There aren't two vectors be orthogonal to [1 1] and each other.
  
 
 
 
 
 */

  //val n = 20  //the n=20 is just for the first question and it is solved with inexact doubles.
 // the * bonus is just about n=5. The ** bonus is for n>=40
 // AHHH the * bonus for any n>=5, so maybe there's no rational solution for n=5.
 // Seeing as they have lots of sqrt(2*5), I'm thinking we should try n=8, as it's a nice power of 2
 
 val n = 9
 val maxElement = 20
 println("maxelement " + maxElement)
 def isSquare(n: Int) = {
  val f = math.sqrt(n).toInt
  f*f == n
 }
 def listOfNSquaresThatSumToASquare(n: Int, sumSoFar : Int = 0, minToUse : Int = 0): Stream[List[Int]] = n match {
  case 0 => if (isSquare(sumSoFar)) Stream(Nil) else Stream.empty
  case _ => for { t <- ((minToUse + 1) to maxElement).toStream
  rest <- listOfNSquaresThatSumToASquare(n-1, sumSoFar+t*t, t)
  } yield t :: rest
 }

 println(listOfNSquaresThatSumToASquare(2).take(10).toList)
 println("3s " + listOfNSquaresThatSumToASquare(3).take(200).toList)
 val nines = listOfNSquaresThatSumToASquare(9,0,0).distinct
 println("9s " + listOfNSquaresThatSumToASquare(9,0,0)./*filter(_.product != 0).*/take(200).toList)

 println("9s nines size " + nines.size)

 // take in a list of ints, and inserts plus and minus (except the first) such that they sum to 0
 def sumsToZero(l : List[Int]): List[List[Int]] = {
  def allPlusOnesAndMinusOnes(k: Int): List[List[Int]] = k match
  {
   case 0 => List(Nil)
   case _ => for{
    first <- List(-1,1)
    rest <- allPlusOnesAndMinusOnes(k-1)
   } yield first :: rest
  }
  val p1m1 = allPlusOnesAndMinusOnes(n-1)
  val s = p1m1.map( p =>  l.head :: l.tail.zip(p).map{ x => x._1 * x._2  } )
  val s1 = s.filter( _.sum == 0)
  s1 ::: s1.map( x => x.map( _ * -1))
 }

 val all = (for {
  x1 <- nines
  x2 <- sumsToZero(x1)
 } yield x2).distinct

 println("all size " + all.size)

 var counter = 0
 val allPairsThatAreOrthogonal = for{
  (x,i) <- all.zipWithIndex
  ysorted <- all.drop(i)
  t = {
   if (counter %100 ==0)
    println("Here 1 " + counter + " i " + i + " x " + x + " ysorted " + ysorted)
   counter = counter+1
  }
  y <- ysorted.permutations
  if (x.zip(y).map{case(x,y) => x*y}.sum == 0 /* do this as a separate step
  && fWithIndicesRational(x, y) */)
  } yield (x,y)

 //println("All pairs size " + allPairs.size)
 //allPairs.foreach(println)
 def fWithIndices( x: List[Double], y: List[Double] ) = {
  //println("in fWithIndices.")
  //println(x)
  //println(y)
  val r =   for {
   i <- 0 until x.size
   j <- 0 until x.size } yield scala.math.abs(x(i) - x(j)) - scala.math.abs(y(i) - y(j))
  r.map(k => k*k).sum
 }
 
 def norm2(x: List[Int]): Int = math.sqrt(x.map(x1 => x1*x1).sum).toInt
 
 // Basically we're doing a least squares on the elements of the distance matrices of x and y, where the metric is the norm2 (absolute value).
 def fWithIndicesRational(x : List[Int], y: List[Int]) : Boolean = {
  val f1 = norm2(x)
  val f2 = norm2(y)
  val r =   for {
   i <- 0 until n
   j <- 0 until n } yield f2 * scala.math.abs(x(i) - x(j)) - f1*scala.math.abs(y(i) - y(j))
  val rsum = r.map(k => k*k).sum
  //  println("rsum " + rsum + " 4*f1*f2*f1*f2 " + 4*f1*f2*f1*f2)
  rsum == f1*f2*f1*f2 * 4.toLong
 }
 
 val solutionsin9 = allPairsThatAreOrthogonal.filter{ case(x,y) => fWithIndicesRational(x,y) }
 
 println("All 9s solutions ")
 solutionsin9.foreach(x => println("Solution FOUND!!! " + x))
 println("Done 9s with maxElement " + maxElement)
 println("Num solutions found " + solutionsin9.size)

/*
 val pathagoreanTriples = for {
  a <- 1 to 20
  b <- 1 to 20
  c <-1 to 20
  if (a*a + b*b == c*c)} yield (a,b,c)
 val pathagoreanQuadruples = for {
  a <- 1 to 20
  b <- a to 20
  c <- b to 20
  d <- c to 20
  if (a*a + b*b + c*c == d*d)} yield (a,b,c,d)

 // particle swarm optimizer for n=5 and congergents on the results

 // or
/*
 val s = Stream.from(500, 25).find{ maxX =>
 
 println("maxX  " + maxX)
 val pathagorean6 = for {
  a <- 0 to maxX
  b <- a to maxX
  c <- b to maxX
  d <- c to maxX
  e <- d to maxX
  //e <- math.max(maxX-25, d) to maxX
  f = math.sqrt(a*a + b*b + c*c + d*d + e*e).floor.toInt
  if (f != 0 && a*a + b*b + c*c + d*d + e*e == f*f)} yield (a,b,c,d,e,f)

 println("pathagorean6.size" + pathagorean6.size)
 val sumsToZero = for {
  (a,b,c,d,e,f) <- pathagorean6
  p1<-List(-1,1)
  p2<-List(-1,1)
  p3<-List(-1,1)
  p4<-List(-1,1)
  if (a + p1*b + p2*c + p3*d + p4*e == 0)
 } yield (a,p1*b,p2*c,p3*d,p4*e,f)
 
 println("sumsToZero.size " + sumsToZero.size)

 
 /*
  (a/b - c/d)^2
  ((ad-bc)/bd)^2
  (ad-bc)^2/bd^2
  So r is the 20 elements of ad-bc. Where d=f2 and b=f1, and a=|xi-xj| and c = |yi-yj|,
  where the x's are of the form {a1,b1...} / f1, which normalizes them.
  
 */
 val xandySolutions = for {
  allCandidatesWithSamef <- sumsToZero.groupBy(_._6).values.filter(_.size >= 2)
  IndexedSeq(x,y) <- allCandidatesWithSamef.combinations(2)
  
  if (y._5 >= x._5)
  //if (y._5 >= maxX -25 )
  List(a2,b2,c2,d2,e2) <- List(y._1, y._2, y._3, y._4, y._5).permutations
  if ( x._1 * a2 + x._2 * b2 + x._3 * c2 + x._4 * d2 + x._5 * e2  == 0 &&
   fWithIndicesRational(IndexedSeq(x._1, x._2, x._3, x._4, x._5, x._6), IndexedSeq(a2, b2, c2, d2, e2, y._6)) )
  } yield (x,y)
 
 println("Solutions for maxx " + maxX)
 println("Number of solutions " + xandySolutions.size)
 xandySolutions.foreach(println)
 
 
 !xandySolutions.isEmpty
 
 /*
 var counter = 0
 val xyOrthogonalAndRationalWithPermutation = for {
  ((a1,b1,c1,d1,e1,f1),i1) <- sumsToZero.zipWithIndex
  t = if (i1%10 == 0)
  {
   println("i = " + i1 + " x = " + a1 + ", " + b1 + ", " + c1 + ", " + d1 + ", " + e1 + ", " + f1 )
  }
  y <- sumsToZero.drop(i1)
  if (y._5 >= maxX -25 )
  List(a2,b2,c2,d2,e2) <- List(y._1, y._2, y._3, y._4, y._5).permutations
   
  t2 = {
   if (counter % 1000000 == 0)
   {
    println("i1 = " + i1 + " counter = " + counter + " x = " + a1 + ", " + b1 + ", " + c1 + ", " + d1 + ", " + e1 + ", " + f1 )
    println("i1                       y = " + a2 + ", " + b2 + ", " + c2 + ", " + d2 + ", " + e2 + ", " + y._6 )
   }
   counter = counter + 1
  }
  if (a1*a2 + b1*b2 + c1*c2 + d1*d2 + e1*e2 ==0)
 } yield (a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,y._6)

 println("xyOrthogonalAndRationalwithPermutation " + xyOrthogonalAndRationalWithPermutation.size)

 val solveInRationalsWithPermutation = xyOrthogonalAndRationalWithPermutation.filter{ case(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)=>
   fWithIndicesRational(IndexedSeq(a1,b1,c1,d1,e1,f1), IndexedSeq(a2,b2,c2,d2,e2,f2) )}
 
 println("solveInRationalsWithPermutation.size  " + solveInRationalsWithPermutation.size)
 solveInRationalsWithPermutation.foreach(x=> println("Solution " + x))
 !solveInRationalsWithPermutation.isEmpty
 */



}
 */
 
/* val xyOrthogonalAndRational = (for {
  (IndexedSeq((a1,b1,c1,d1,e1,f1),(a2,b2,c2,d2,e2,f2)),i) <- sumsToZero.combinations(2).zipWithIndex
  t = if (i%10000 == 0)
  {
   println("i = " + i + " x = " + a1 + ", " + b1 + ", " + c1 + ", " + d1 + ", " + e1 + ", " + f1 )
   println("    " + i + " y = " + a2 + ", " + b2 + ", " + c2 + ", " + d2 + ", " + e2 + ", " + f2 )
  }
  if ( a1*a2 + b1*b2 + c1*c2 + d1*d2 + e1*e2 ==0)
 } yield (a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)).toList
 

 println("xyOrthogonalAndRational " + xyOrthogonalAndRational.size)
 // With maxX =100 this returns size==1551 but no solutions for the f=4 criteria.
 
 val solveInRationals = xyOrthogonalAndRational.filter{ case(a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2)=>
   fWithIndicesRational(IndexedSeq(a1,b1,c1,d1,e1,f1), IndexedSeq(a2,b2,c2,d2,e2,f2) )}
 
 println("solveInRationals.size  " + solveInRationals.size)
 solveInRationals.foreach(x=> println("Solution " + x))
 */
 def f( x: List[Double], y: List[Double] ) = {
  // DO NOT USE THIS FUNCTION
  // combinations de-dublicates entries, omitting entries that it has seen before.
  // Definitely not what we want when adding terms in a matrix.
  val r = for { List( (xi, yi), (xj, yj) )  <- (x zip y).combinations(2)} yield scala.math.abs(xi - xj) - scala.math.abs( yi - yj)
	println(r)
  2 * r.map(x => x*x).sum
 }

 def gcd(a: Long, b:Long) : Long = if (a == 0) b else gcd(b%a,a)

 def fConvergents(convergents: List[(Int,Int)]) = {
  val (x,y) = convergents.splitAt(n)
  val gcdX = x.map(_._2.toLong).reduce(gcd)
  val lcmX = x.map(_._2.toLong).product / gcdX
  val gcdY = y.map(_._2.toLong).reduce(gcd)
  val lcmY = y.map(_._2.toLong).product / gcdY
 // worry about whether or not we've got negatives
  
  val gcdXY = gcd(lcmX,lcmY)
  val lcmXY = lcmX * lcmY / gcdXY
  
  val xOverLCM = x.map( ab => ab._1 * lcmX / ab._2)
  val yOverLCM = y.map( ab => ab._1 * lcmY / ab._2)
   
  val r =   for {
   i <- 0 until x.size
   j <- 0 until x.size } yield scala.math.abs(xOverLCM(i) - xOverLCM(j))*lcmXY / lcmY -
                               scala.math.abs(yOverLCM(i) - yOverLCM(j))*lcmXY / lcmX
   
  r.map(k => k*k).sum == lcmXY * lcmXY * 4
 }
 


 def calculateNormalizedVectors( in: List[Double]) =
 {
  // in is of size 2*n - 3, which is (n-1) + (n-2)
  val (initialUnnormalizedXNminus1terms, initialUnnormalizedYNminus2terms) = in.splitAt(n-1)
  val lastx = - initialUnnormalizedXNminus1terms.sum
  val completeUnnormalizedX = initialUnnormalizedXNminus1terms :+ lastx
  val normalizedx = completeUnnormalizedX.map( x => x / scala.math.sqrt(completeUnnormalizedX.map( y=> y*y).sum ))

  // xa is x(n-2)
  // xb is x(n-1)
  val yNMinus1 = 1.0 / (normalizedx(n-1) -normalizedx(n-2)) *( normalizedx(n-1) * -initialUnnormalizedYNminus2terms.sum +
   (normalizedx zip initialUnnormalizedYNminus2terms).map{case(xi, yi)=> xi*yi}.sum)

  val yN       = 1.0 / (normalizedx(n-1) -normalizedx(n-2)) *( normalizedx(n-2) *  initialUnnormalizedYNminus2terms.sum -
   (normalizedx zip initialUnnormalizedYNminus2terms).map{case(xi, yi)=> xi*yi}.sum)
  
  val completeUnnormalizedY = initialUnnormalizedYNminus2terms :+ yNMinus1 :+ yN
  val normalizedy = completeUnnormalizedY.map( x => x / scala.math.sqrt(completeUnnormalizedY.map( y=> y*y).sum ))
  //assert( normalizedx.size == 20 && normalizedy.size == 20, "Not 20" )
  //println( "x.sum "  + normalizedx.sum )
  //println( "x magnitude "  + scala.math.sqrt(normalizedx.map(x=> x*x).sum ))
  //println( "y.sum "  + normalizedy.sum )
  //println( "y magnitude "  + scala.math.sqrt(normalizedy.map(x=> x*x).sum ))
  //println( "x dot y "  + (normalizedx zip normalizedy).map{ case (xi,yi) => xi*yi}.sum )
  
  (normalizedx, normalizedy)
 }

 def functionToOptimize( in: List[Double]): Double =
 {
  val (normalizedx, normalizedy) = calculateNormalizedVectors(in)
  fWithIndices(normalizedx, normalizedy)
 }
 
 // Particle Swarm Optimization ???
 
 // Lagrangian Multiplier ???
 def hadamaard(n: Int): List[List[Int]] = n match {
  case 1 => List( List(1))
  case _ => {
   val hNMinus1 = hadamaard(n-1)
   // The kroneker product with
   // [[1 1]
   //  [1 -1]]
   hNMinus1.map( l => l ::: l) ::: hNMinus1.map( l => l ::: l.map( - _ ))
  }
 }
 hadamaard(1)
 hadamaard(2)
 println("hadamaard(3)(1) " + (0 +: hadamaard(3)(1) :+ 2))
 println("hadamaard(3)(2) " + (0 +: hadamaard(3)(2) :+ 2))
 
 val xIndexedSeq = (0 +: hadamaard(3)(1) :+ 2).map(_.toLong).toIndexedSeq
 val yIndexedSeq = (0 +: hadamaard(3)(2) :+ 2).toIndexedSeq
 val xListDouble = (0 +: hadamaard(3)(1)).map( _.toDouble / 2.0)
 val yListDouble = (0 +: hadamaard(3)(2)).map(_.toDouble / 2.0)
 
 
 
// println( fWithIndicesRational(xIndexedSeq, yIndexedSeq))
// println( fWithIndices(xListDouble, yListDouble))
 
  
 
 
 hadamaard(3)
 
 /* For n=20 , find a pair of x,y that are 0.0000001-close to being an optimal solution. Give your solution in the exact format given above.
 Maybe concatonate n=16 and n=4.
 */
 val h4 = hadamaard(5)
 val h2 = hadamaard(3)
 h4.size
 h2.size
 val xInt = h4(1) ::: h2(1)
 val yInt = h4(2) ::: h2(2)
 val x = xInt.map( _.toDouble / (scala.math.sqrt(xInt.map(k => k*k).sum)))
 val y = yInt.map( _.toDouble / (scala.math.sqrt(yInt.map(k => k*k).sum)))
 x.sum
 y.sum
 x.map(k=> k*k).sum
 y.map(k=> k*k).sum
 (x zip y).map{ k => k._1 * k._2}.sum
 
 x.size
 y.size
 
 x zip y
 (x zip y).combinations(2).toList
 (x zip y).combinations(2).toList.size
 
 
 
 f(x, y)
 fWithIndices(x, y)
 
 
 /*
  for each particle i = 1, ..., S do
    Initialize the particle's position with a uniformly distributed random vector: xi ~ U(blo, bup)
    Initialize the particle's best known position to its initial position: pi ← xi
    if f(pi) < f(g) then
        update the swarm's best known position: g ← pi
    Initialize the particle's velocity: vi ~ U(-|bup-blo|, |bup-blo|)
while a termination criterion is not met do:
    for each particle i = 1, ..., S do
        for each dimension d = 1, ..., n do
            Pick random numbers: rp, rg ~ U(0,1)
            Update the particle's velocity: vi,d ← w vi,d + φp rp (pi,d-xi,d) + φg rg (gd-xi,d)
        Update the particle's position: xi ← xi + vi
        if f(xi) < f(pi) then
            Update the particle's best known position: pi ← xi
            if f(pi) < f(g) then
                Update the swarm's best known position: g ← pi
  */
  
 // Initial particles
 val numParticles = 50000
 
 Stream.from(0).find( trial => {
 println("trial " + trial)
 // Initialize the particle's position with a uniformly distributed random vector: xi ~ U(blo, bup)
 val positions = List.fill(numParticles)( (0 until 2*n - 3 ).map( x => -1.0 + 2* scala.util.Random.nextDouble()).toList)
 // Initialize the particle's best known position to its initial position: pi ← xi
 val bestKnownPositions = positions
 // update the swarm's best known position: g ← pi
 val swarmsBestKnowPosition = positions.minBy( functionToOptimize )
 // Initialize the particle's velocity: vi ~ U(-|bup-blo|, |bup-blo|)
 val velocities = List.fill(numParticles)(  (0 until 2*n - 3 ).map( x => -.01 + .02 * scala.util.Random.nextDouble()).toList)
 val omega = 0.3
 val phip = 0.3
 val phig = 0.3

 def continuedFraction(x: Double): Stream[Int] = x.floor.toInt #:: (if (x % 1 == 0) Stream.empty else continuedFraction( 1.0 / (x % 1) ))
  
 /*
 println("cont fract " + continuedFraction(3.25156).take(10).toList)
 val solList = List(0.4826727578443017, -0.36171577056530313, 0.11706262290959807, 0.17299289120475406, 0.4033221444880253, 0.35879191709395564, -0.09750872262782768, -0.13580060616795356, -0.14112453495795527, 0.3675469720357314, 0.1099582218583469, 0.11192478746217921, 0.005902791295483172, -0.31556658466773985, -0.419073566419609, 0.40707614906551526, 0.07517133488877958, -0.36223356925558126, 0.4589037009821944, 0.36039802544496835, -0.5033552690746049, -0.013579798836871096, 0.04359009247921047, 0.2792274590044055, 0.23366765661829175, -0.2330929125923857, -0.27222258566250096, -0.27770653060782097, 0.24264481111014927, -0.02086689638582433, -0.018879024310296387, -0.12724013040853296, -0.45611891786362696, -0.56199557440238, 0.2830784062508056, -0.056421674472704256, -0.503892566009515)
 
 solList.foreach( s =>
 {
  println(s + " " + continuedFraction(math.abs(s)).take(25).toList)
  println("converge " + convergents(math.abs(s)).take(25).toList)
 })
  
 */
  
 /*
  Consider the convergents c_n=A_n/B_n of a simple continued fraction [b_0;b_1,b_2,...], and define
 A_(-1)	=	1
 B_(-1)	=	0
 A_0	=	b_0
 B_0	=	1.

 Then subsequent terms can be calculated from the recurrence relations
 A_k	=	b_k*A_(k-1)+A_(k-2)
 B_k	=	b_k*B_(k-1)+B_(k-2).
*/
 def convergents(x: Double): Stream[(Int,Int)] = (1,0) #:: (continuedFraction(x).head,1) #::
 {
  // so the convergents(x) is the k-2 term and the convergents(x).drop(1) is the k-1 term
  // continuedFraction(x).drop(1) will start at the b1 term since the b0 term is already taken care of with the "head" line above.
  convergents(x).zip(convergents(x).drop(1)).zip(continuedFraction(x).drop(1)).map{ case(((akm2,bkm2),(akm1,bkm1)),bk) =>
  (bk*akm1+akm2, bk*bkm1+bkm2)
  //(t._2 * t._1._1._1 + t._1._2._2, t._2 * t._1._2._1 + t._1._2._2)
  }
 }
 println("conveg "  + convergents(3.25156).take(10).toList)
 
 @tailrec
 def particleSwarmOptimizer( positions: List[List[Double]], velocities: List[List[Double]], bestKnownPositions: List[List[Double]], best: List[Double]): List[Double] =
 {
  // for each particle i = 1, ..., S do
  val newVelocities = (velocities, positions, bestKnownPositions).zipped.toList.map{ case(vi, xi, pi) =>
   //  for each dimension d = 1, ..., n do
   (vi, xi, pi).zipped.toList.zip(best).map{ case((vid, xid, pid),gd) =>
    //       Pick random numbers: rp, rg ~ U(0,1)
    val rp = scala.math.random()
    val rg = scala.math.random()
    //       Update the particle's velocity: vi,d ← w vi,d + φp rp (pi,d-xi,d) + φg rg (gd-xi,d)
    omega * vid + phip * rp * (pid - xid) + phig * rg * (gd - xid)
   }
  }
  // Update the particle's position: xi ← xi + vi
  val newPositions = (velocities, positions).zipped.toList.map{ case(vi, xi)  =>
   (xi zip vi).map{ case(xid, vid) => xid + vid }
  }
  //  if f(xi) < f(pi) then
  //     Update the particle's best known position: pi ← xi
  val newBestKnownPositions = (positions zip bestKnownPositions).map{ case(xi, pi) => if (functionToOptimize(xi) < functionToOptimize(pi)) xi else pi }
  //  if f(pi) < f(g) then
  //     Update the swarm's best known position: g ← pi
  val newswarmsBestKnowPosition = newBestKnownPositions.minBy( functionToOptimize )
  println("Best " + newswarmsBestKnowPosition + " with value " + functionToOptimize(newswarmsBestKnowPosition))
  // while a termination criterion is not met do:
  if ( functionToOptimize(newswarmsBestKnowPosition) <= 4 + 0.0000001 )
   newswarmsBestKnowPosition
  else
   particleSwarmOptimizer(newPositions, newVelocities, newBestKnownPositions, newswarmsBestKnowPosition)
 }
 
  val sol = particleSwarmOptimizer(positions, velocities, bestKnownPositions, positions(0))
  println("solution Found pso" + sol)

  // Let's multiply everything by sqrt(10)
  val (normalizedx, normalizedy) = calculateNormalizedVectors(sol)
  val unnormalizedX = normalizedx.map( _ * math.sqrt(1))
  val unnormalizedY = normalizedy.map( _ * math.sqrt(1))
  
  val (sortedX, sortedY) = unnormalizedX.zip(unnormalizedY).sortBy(_._1).unzip
  
  val normalizedSols = sortedX ::: sortedY
  println("x = " + sortedX)
  println("y = " + sortedY)
  
  println("sortedX")
  (0 until n).map{ i =>
   println( (i+1 until n).map{ j =>
    sortedX(j) - sortedX(i)
   }.mkString(", "))
  }
  println("sortedY")
  (0 until n).map{ i =>
   println( (i+1 until n).map{ j =>
    sortedY(j) - sortedY(i)
   }.mkString(", "))
  }
  println("X - Y")
  (0 until n).map{ i =>
   println( (i+1 until n).map{ j =>
    (sortedX(j) - sortedX(i)) - (sortedY(j) - sortedY(i))
   }.mkString(", "))
  }
  println("X - |Y|")
  (0 until n).map{ i =>
   println( (i+1 until n).map{ j =>
    (sortedX(j) - sortedX(i)) - math.abs(sortedY(j) - sortedY(i))
   }.mkString(", "))
  }
  println("(X - |Y|)^2")
  (0 until n).map{ i =>
   println( (i+1 until n).map{ j =>
    ((sortedX(j) - sortedX(i)) - math.abs(sortedY(j) - sortedY(i)))*
    ((sortedX(j) - sortedX(i)) - math.abs(sortedY(j) - sortedY(i)))
   }.mkString(", "))
  }
  
  
  
   
  val continuedFractionsOfSolutions = normalizedSols.map(s => continuedFraction(math.abs(s)).take(20).toList)
  val convergentsOfSolutions = normalizedSols.map(s => convergents(math.abs(s)).take(21).toList.map(t => if (s >= 0) t else (-t._1,t._2)))
  
  normalizedSols.foreach(s => {
  println("Num s " + s + " continued fraction = " + continuedFraction(math.abs(s)).take(20).toList)
  println("convergents " + convergents(math.abs(s)).take(21).toList)
  })
  
  // hmmm, maybe take while the denominator is less than 500?
  val bestConvergentsIndecies = continuedFractionsOfSolutions.map( c => c.zipWithIndex.takeWhile( _._1 <= 5 ).last._2)
  val bestConvergents = convergentsOfSolutions.zip(bestConvergentsIndecies).map( c => c._1(c._2 + 1))
  println(bestConvergents)
  
  /* also maybe to prove it we find the 10x5 matrix D
  [
  -1 1 0 0 0
  -1 0 1 0 0
  -1 0 0 1 0
  -1 0 0 0 1
  0 -1 1 0 0
  0 -1 0 1 0
  0 -1 0 0 1
  0 0 -1 1 0
  0 0 -1 0 1
  0 0 0 -1 1]
  x is a 5x1 column vector
  if x is sorted then
  D x is the 10x1 entries of |xj-xi|
  D y is the 10x1 for the ys.
  Dx - Dy is the 10x1 entries for |xj-xi| - (yj-yi). just missing the absolute value signs around the y entries
  Lets output the sorted x and ys.
  
  sortedX
0.6175718619828015, 0.7825489097391602, 0.928164825863335, 1.3726097532761443
0.16497704775635885, 0.3105929638805336, 0.755037891293343
0.14561591612417477, 0.5900608435369842
0.4444449274128094 looks like 4/9th here.

sortedY
0.6175665151941129, 0.7824870774271344, 0.9280876647456278, -0.20856516004547299
0.16492056223302154, 0.3105211495515149, -0.8261316752395859
0.14560058731849335, -0.9910522374726074
-1.1366528247911007

X - Y
5.346788688576076E-6, 6.18323120258335E-5, 7.716111770728062E-5, 1.5811749133216173
5.648552333731294E-5, 7.181432901870455E-5, 1.5811695665329288
1.5328805681419366E-5, 1.5811130810095917
1.58109775220391
  
  So the negatives in the sortedY need to be converted to positive, but other than that, look at those zeroes!!!
  And 1.5811 happens to be sqrt(2.5)!!!
  looking at entry (0,4) we have (1.3726-.208565)^2 =1.355 perhaps exactly?
  second row is 0.00505444214916
  third row is .1607
  last row is .479149
  sums to 2 as expected.
  
  OK. Definitely something up with sqrt(2.5)
  X - Y
1.5811391514855888, 1.581138236037277, 1.5811180729148036, 1.5811598585179465
-9.154483114892642E-7, -2.1078570785038053E-5, 2.070703235779181E-5
-2.016312247353491E-5, 2.1622480669294952E-5
4.1785603142829864E-5
  
  
  0.6324560037047924 = sqrt(.4) = sqrt(4)/sqrt(10) = 2/sqrt(10)... hmmm so maybe we started with a 2, and got a sum of squares to be 10.

 so if x and y are solutions with f(x,y)=4. Then what is f(x::x, y::y) or f(x::y, y::x)?
 so if |x| = 1, then |x :: x| = sqrt(2)
 so need to normalize by dividing each term by sqrt(2).
 then xj-xi will turn into xj/sqrt(2) - xi/sqrt(2). same with the ys.
 so when we go to square each term we get 1/2 * previous term.
 but there are 10 choose 2 terms = 10*9/2 = 45, whereas last time there were 5 choose 2 = 5*4/2=10 terms.
 But 5 of those 45 terms will be 0. so really there will be only 4 times more terms. So f will increase by (4/2) = 2. Hmmm.
   
  Let's start compiling a database of suspected exact values
  0.45325 = 1.433333 / sqrt(10) = 1.4 +.033333... = 1.4 + 1/30 = 14/10 + 1/30 = 42/30 + 1/3 = 43/30 / sqrt(10)
  1.5811 = sqrt(2.5) = sqrt(25/10) = sqrt(25)/sqrt(10) = 5/sqrt(10).
  0.6324 = 2/sqrt(10)
  
  after multiplying by sqrt(10)
  x = List(-1.2137674653999666, -1.1526076743240286, -1.0680294188615056, 1.434350804614065, 2.0000537539714363)
  y = List(-0.21368758130365778, -0.15261902270709785, -0.06820962796051508, 2.4344624728542974, -1.9999462408830266)
  so y is x+1
  except for the last one which is x-4
  so we have x+1 4 times, and x-4 once.
  makes sense that they are orthoganinal. or does it? THis is addition, not multiplication. Maybe with logrithms.

 x1 x2 x3 x4 x5
 +1 +1 +1 +1 -4
 
 y1 y2 y3 y4 y5
  
  so when we're doing something like x5-x2 - (y5-y2) that's the same as
  (y2 - x2) - (y5-x5), which is +1 - (-4) = +5! Ah mystery solved.
  
  x = List(-1.7101667490657082, -1.2388592862304812, -0.2638435489238919, 1.2129386305032714, 1.9999309537168093)
  y = List(-0.7102444914798627, -0.2386232378004906, 0.7361522150868295, 2.2127845519246248, -2.0000690377311012)
  
  
  For n=8
  x = List(-0.6773002396049101, -0.17370041685394394, -0.17249109038300425, 0.026551592297003206, 0.06091009266215722, 0.10794211068170845, 0.1666495322592865, 0.6614384189417029)
  0.6614 where the last number is sqrt(7)/4
  0.1666 second last number looks like 1/6
  
  n=6
  x = List(-0.6239792922778165, -0.33044703635617056, -0.02560144669598142, 0.04854566344150747, 0.28600093649965286, 0.6454811753888082)
  y = List(0.365755636215432, 0.07223564504905189, -0.23257468973835788, -0.30675175349037215, -0.5441781107983253, 0.6455132727625714)
  0.6454811753888082 last number possibly sqrt(5/12)

  n=7
  x = List(-0.3312764041868526, -0.3152859020420526, -0.2896181037853365, -0.16314665910636672, -0.05417496097984313, 0.4988508466785323, 0.6546511834219192)
  y = List(0.11304336971340134, 0.097027219988428, 0.07142057373638547, -0.05504749001513588, -0.16402446308390176, -0.7170753672672858, 0.6546561569281085)
  last number
  0.6546511834219192 = sqrt(3/7)


I'm thinking n=9 or n=16 have a shot.
  GOOD!
  n=9
  x = List(-0.6666606030310057, -0.5732174352593354, 0.1161123511525026, 0.15690908882266363, 0.16786741227995305, 0.1799210661856719, 0.1886480419602938, 0.20769311097441756, 0.2227269669148386)
  y = List(0.6666727285715058, -0.7398718793994516, -0.0505332366305847, -0.009780339465215483, 0.0011702353768068678, 0.013231818187785793, 0.02199876797223879, 0.041070811782883784, 0.05604109360403077)
  
  clearly starting with a rational number 2/3.
  n=16
  x = List(-0.6846532176934222, -0.2208468294932238, -0.21620796506376472, -0.18320129327611587, -0.1401529957493335, -0.10452300008756846, -0.04144524522683875, 0.003489132441664219, 0.05382771779094194, 0.10613761769162078, 0.15605334845302998, 0.21664114254526468, 0.23566062979987543, 0.24233515362939584, 0.24507975393379647, 0.331806050304678)
  y = List(0.6846531749534587, -0.31213163443220304, -0.3074848121796878, -0.274474049242384, -0.23146138157832513, -0.19581034184920626, -0.13272605475528546, -0.08782287895747372, -0.03745822648310833, 0.01485207779735432, 0.06478976610260288, 0.12533197039613303, 0.14436552492446106, 0.15106464216791593, 0.15377800955038037, 0.24053421358536742)
  first is sqrt(120)/16
  120=2*2*2*3*5
  the difference between x and y is
  9.128480493897925e-02 hmmm not sure.
  but x-y has lots of
  1.46059119758586 =8/sqrt(30)
  hhmmmm, since we're dealing with a triangular number of terms, maybe we're looking for a square triangular number.
  1,36,1225.
  So for 36, is that when n=9?
  We get 9 choose 2? 9*8/2=36.
  
 x = List(-0.38608338993445174, -0.25921276207158733, -0.2459379669821293, -0.19712752045737514, -0.15934200428078144, -0.10631085330081594, -0.05938395099051539, -0.055853136362840734, -0.01787933873699289, 0.0087547406902319, 0.0566675164590497, 0.09350539181500495, 0.09689566892851238, 0.24923514659052048, 0.2974195321793293, 0.6846529264548411)
 y = List(-0.29479094090817237, -0.1679437788715896, -0.15462919465959546, -0.10582362674412166, -0.06806060106567154, -0.015034431431525747, 0.03187639742501868, 0.0354466795198869, 0.07339805989450456, 0.10002277689211168, 0.14797575136347213, 0.1847961111612717, 0.18819045170281976, 0.34051949065639125, 0.3887103213018177, -0.6846534662366182)
  
 x = List(-0.6846496911015393, -0.3393625586896019, -0.15781444320047616, -0.13961196279373347, -0.13243128599559217, -0.11324620821724043, -0.07140039137505665, 0.05913444264428179, 0.1230673320436571, 0.1260164443966816, 0.18704756219557053, 0.195873081563505, 0.19661636306745672, 0.22187495503869784, 0.24701245854612622, 0.2818739018772633)
 y = List(-0.6846567015452955, 0.43064144647654423, 0.2490952019738561, 0.2309013501038451, 0.22370342746142194, 0.2045388368616581, 0.16269766959397544, 0.03214317869024774, -0.03176940909305535, -0.03471170301438978, -0.09579678422136402, -0.1045598462863887, -0.10532358599931345, -0.13057934930118592, -0.1557241652398775, -0.1905995664606785)
  
 diff x pretty equals diff y except for the first term
  
  OK, new approach.
  For the n=3 case, we can use octave to parameterize the sinosoidal set of solutions to x and y.
  Then there are 3 choose 2 = 3 terms in f, which are all |1/sqrt(3) * sin(t - theta)| or similar. We can plot that and see where the optimum occurs and hopefully why.
  Super hopefully we'll see how this relates to rational numbers. But might have something to do with modular forms. Yikes!
  
  
  Looks like the function f is actually a "norm" of the two vectors?
  Almost
  https://en.wikipedia.org/wiki/Euclidean_distance_matrix
  Definitely
  https://en.wikipedia.org/wiki/Distance_matrix
  

*/
  
  
  // Really all we want is to know if the convergents also sum to 0, and are orthogonal
  
   fConvergents(bestConvergents)
  
  })
 
  
  
  
//  val Best = List(0.4826727578443017, -0.36171577056530313, 0.11706262290959807, 0.17299289120475406, 0.4033221444880253, 0.35879191709395564, -0.09750872262782768, -0.13580060616795356, -0.14112453495795527, 0.3675469720357314, 0.1099582218583469, 0.11192478746217921, 0.005902791295483172, -0.31556658466773985, -0.419073566419609, 0.40707614906551526, 0.07517133488877958, -0.36223356925558126, 0.4589037009821944, 0.36039802544496835, -0.5033552690746049, -0.013579798836871096, 0.04359009247921047, 0.2792274590044055, 0.23366765661829175, -0.2330929125923857, -0.27222258566250096, -0.27770653060782097, 0.24264481111014927, -0.02086689638582433, -0.018879024310296387, -0.12724013040853296, -0.45611891786362696, -0.56199557440238, 0.2830784062508056, -0.056421674472704256, -0.503892566009515)// with value 4.000000098011656
 
 println("non bonus solution")
 println(calculateNormalizedVectors( List(0.4826727578443017, -0.36171577056530313, 0.11706262290959807, 0.17299289120475406, 0.4033221444880253, 0.35879191709395564, -0.09750872262782768, -0.13580060616795356, -0.14112453495795527, 0.3675469720357314, 0.1099582218583469, 0.11192478746217921, 0.005902791295483172, -0.31556658466773985, -0.419073566419609, 0.40707614906551526, 0.07517133488877958, -0.36223356925558126, 0.4589037009821944, 0.36039802544496835, -0.5033552690746049, -0.013579798836871096, 0.04359009247921047, 0.2792274590044055, 0.23366765661829175, -0.2330929125923857, -0.27222258566250096, -0.27770653060782097, 0.24264481111014927, -0.02086689638582433, -0.018879024310296387, -0.12724013040853296, -0.45611891786362696, -0.56199557440238, 0.2830784062508056, -0.056421674472704256, -0.503892566009515)))
 /*
 (x=List(0.268640424563741, -0.2013195826713901, 0.0651533615848355, 0.09628238383974673, 0.22447637735994244, 0.19969225810181426, -0.054270277780743274, -0.07558233172285324, -0.07854546247189488, 0.20456504538556688, 0.06119927616430542, 0.0622938045174394, 0.0032853073515312636, -0.17563440220220036, -0.2332431217150242, 0.22656573783683193, 0.04183799270075857, -0.201607773081423, 0.25541131763126323, -0.6892003353922475),
 y=List(0.1960921549298141, -0.27387502827256055, -0.007388753071406746, 0.02371731964203968, 0.1519273422326533, 0.12713830567497741, -0.1268255881054685, -0.14811599863017785, -0.15109980682534363, 0.1320227652035022, -0.011353654543310623, -0.010272055612423588, -0.0692312099505931, -0.24817378340989527, -0.3057811515740644, 0.15402263824082452, -0.03069896878164355, -0.2741673709225338, 0.1828583048412401, 0.6892045389343702))
 interesting that the last values of x and y are so similar. Looks like they are trying to be sqrt(19/40)?
 */
 */
 }                                                //> main: (args: Array[String])Unit
}