object ponder {
def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
}                                                 //> time: [R](block: => R)R

 def intToListInt(i: Int): List[Int] = if (i==0) Nil else intToListInt(i/10) :+  i % 10
                                                  //> intToListInt: (i: Int)List[Int]
 intToListInt(52634)                              //> res0: List[Int] = List(5, 2, 6, 3, 4)
 
 def solve(n: Int, d: Int) = {
 def isPrime(num: Int): Boolean = Stream.from(2).takeWhile(x => x*x <= num).forall( x => num % x != 0)
 val listOfPrimes = (0 until scala.math.pow(10, d).toInt).map( isPrime )
 val dDigitPrimes = listOfPrimes.zipWithIndex.filter(_._2 > scala.math.pow(10,d-1)).filter(_._1).map(_._2)
 .filter{ p =>
  val digits = intToListInt(p)
  digits.distinct.size == digits.size
 }
 println("dDigitPrimes "  + dDigitPrimes.size)
 println(dDigitPrimes.take(5))
 
 def scoreIfConstructable(digits: IndexedSeq[Int], circle: IndexedSeq[Int]): Int = {
/*  if ( !(digits.toSet -- circle).isEmpty)
 	 None
 	else
   Some(
  */  digits.map( digit => circle.indexOf(digit) ).sliding(2).map{ pair=>
     val diff = scala.math.abs(pair(1) - pair(0))
     scala.math.min(diff, n - diff)
    }.sum
   //)
 }
 println("here1")
 //assert( scoreIfConstructable(List(2,4,1,0,3), List(4,7,3,6,2,0,1)) == Some(8))
 var counter = 0
 
/* def scoreCircle2( digits: IndexedSeq[Int], circle: IndexedSeq[Int]): Int = {
  val score = digits.map{ p =>
   scoreIfConstructable(intToListInt(p), circle)
  }.sum
  if (counter % scala.math.pow(10,4) == 0)
   println(circle + " " + score + " " + counter)
  counter = counter+1
  score
 }
 */
 println("here2")
 //assert( scoreCircle2((List(4,7,3,6,2,0,1),0)) == 1882)
 println("here3")
 //val circles = (0 to 9).toList.combinations(n).flatMap(_.permutations).toStream
 
 val scores /*circles*/ = for{
  m <- (0 to (10-n)).par
  combo <- ((m+1) to 9).combinations(n-1)
  digits = dDigitPrimes.filter(digits => (intToListInt(digits).toSet -- combo - m).isEmpty)
  rest <- combo.permutations
  if (rest.head < rest.last)
  circle = m +: rest
  score = digits.map( dig=> scoreIfConstructable(intToListInt(dig).toIndexedSeq, circle)).sum
 } yield (circle, score)
//  .filter( x => x.head < x.last )
 //println("circles size " + circles.length)
 println("circles size " + scores.length)
 // So 9P7 + 8P7 + 7P7 == 226800
 
 def intListToInt(digits: List[Int]): Int = digits.zipWithIndex.map{ case (int, i) => int* scala.math.pow(10, digits.size - i -1)}.sum.toInt
 
 intListToInt(List(2,3))
 assert( intListToInt(List(2,4,1,0,3)) == 24103 )
 BigInt(intListToInt(List(2,3))).isProbablePrime(2)
 
 def scoreDigits(digits: List[Int], circle: List[Int]): Int = {
  val indeces = digits.map( digit => circle.indexOf(digit))
  val slidingIndeces = indeces.sliding(2)
  slidingIndeces.map{ pair =>
   val diff = pair(1)-pair(0)
   // pair(1) and pair(0) can each be from 0 to n-1
   // so their difference can be from -(n-1) to +(n-1), excluding 0, which is -n+1 to n-1.
   // We want to turn that into a value from 1 from floor(n/2) since the pair of indeces are distinct.
   val absdiff = scala.math.abs(diff)
   scala.math.min(absdiff, circle.size - absdiff)
  }.sum
 }
 
 scoreDigits(List(2,3), List(1,3,2))
 assert( scoreDigits(List(2,4,1,0,3), List(4,7,3,6,2,0,1)) == 8)
 
 def scoreCircle( circle: (List[Int],Int)): Int = {
  // Taking combinations(d) ensures we will have all distinct digits since circle is composed of distinct digits too.
  val digits = circle._1.combinations(d).toList.flatMap( _.permutations )
  val digitsWithoutLeadingZero = digits.filter( _.head !=0 )
  val digitsThatArePrime = digitsWithoutLeadingZero.filter{ digit =>
   listOfPrimes(intListToInt(digit))
   //BigInt(intListToInt(digit)).isProbablePrime(10)
  }
  val scores = digitsThatArePrime.map( digit => scoreDigits(digit, circle._1))
  if (circle._2 % scala.math.pow(10,4) == 0)
   println(circle + " " + digits.size + " " + digitsWithoutLeadingZero.size + " " + digitsThatArePrime.size + " " + scores.sum)
   
  if (scores.sum == 1882)
   println("1882 with " + circle)
  scores.sum
 }
/*
 println(scoreCircle(List(4,7,3,6,2,0,1)))
 if (d==5)
  assert( scoreCircle(List(4,7,3,6,2,0,1)) == 1882)
*/
 println("1")
// time {circles.zip(circles.zipWithIndex map scoreCircle) }

 println( "2")
// val scores = time{circles.zip(circles.zipWithIndex map scoreCircle2)}
 /*
 List(0, 1, 2, 3, 4, 5, 6) 2520 2160 143 1154
 List(0, 1, 2, 3, 4, 5, 7) 2520 2160 217 1719
 List(0, 1, 2, 3, 4, 5, 8) 2520 2160 137 1080
 List(0, 1, 2, 3, 4, 5, 9) 2520 2160 218 1739
 List(0, 1, 2, 3, 4, 6, 7) 2520 2160 231 1821
 */
 // 7 choose 5, = digits = 7*6/2 = 7*3 = 21
 // without the leading 0 means the first one can be 1 of 6.
 // the remaining 4 can be composed of the remaining 6.
 // so 6 * (6 choose 4) = 6 * (6*5/2) = 6* 15 =90
 // Paradox. Mistake
 
 // Why doesn't
 // 7 choose 5 = 7 * (6 choose 4) ?
 // 7 choose 5 = 6 choose 4   +   6 choose 5
 // We want the permutations not the combinations!!!
                                                   
 /*
 circles.maxBy(scoreCircle)
 circles.minBy(scoreCircle)
 */
 
  
 println("Max score when n=" + n + " and d=" + d + " is " + scores.maxBy(_._2) + " with the perms")
 // Max score when n=7 and d=5 is (List(1, 2, 5, 4, 9, 3, 7),3051) with the perms
 
 
 // Max score when n=7 and d=5 is (List(1, 2, 3, 4, 5, 7, 9),2938)
 // Max score when n=8 and d=6 is (List(1, 2, 3, 4, 6, 7, 8, 9),22833)

 println("Min score when n=" + n + " and d=" + d + " is " + scores.minBy(_._2) + " with the perms")
 // Min score when n=7 and d=5 is (List(0, 1, 4, 5, 8, 2, 6),446) with the perms
 // Min score when n=7 and d=5 is (List(0, 1, 2, 4, 5, 6, 8),473)
 // Min score when n =8 and d= 6 is (List(0, 1, 2, 4, 5, 6, 8, 9),8488)
 (scores.minBy(_._2), scores.maxBy(_._2))
 
 }                                                //> solve: (n: Int, d: Int)((scala.collection.immutable.IndexedSeq[Int], Int), 
                                                  //| (scala.collection.immutable.IndexedSeq[Int], Int))
 
 time{ println(solve(8,6)) }                      //> dDigitPrimes 10239
                                                  //| Vector(102359, 102367, 102397, 102437, 102497)
                                                  //| here1
                                                  //| here2
                                                  //| here3
                                                  //| circles size 113400
                                                  //| 1
                                                  //| 2
                                                  //| Max score when n=8 and d=6 is (Vector(1, 8, 2, 3, 4, 6, 7, 9),23209) with t
                                                  //| he perms
                                                  //| Min score when n=8 and d=6 is (Vector(0, 1, 4, 8, 2, 5, 9, 6),8265) with th
                                                  //| e perms
                                                  //| ((Vector(0, 1, 4, 8, 2, 5, 9, 6),8265),(Vector(1, 8, 2, 3, 4, 6, 7, 9),2320
                                                  //| 9))
                                                  //| Elapsed time: 907584675579ns
 // 7,5 took 39 seconds!
 // 8,6 took 839 seconds.
 
/*dDigitPrimes 2529
Vector(10243, 10247, 10253, 10259, 10267)
*/
 /*
  circles size 43200
  (List(0, 1, 2, 3, 4, 5, 6),0) 2520 2160 143 1154
  Max score when n=7 and d=5 is (List(1, 2, 5, 4, 9, 3, 7),3051) with the perms
  Min score when n=7 and d=5 is (List(0, 1, 4, 5, 8, 2, 6),446) with the perms
  ((List(0, 1, 4, 5, 8, 2, 6),446),(List(1, 2, 5, 4, 9, 3, 7),3051))
 */
 
 
 //println(solve(8,6))
 /*circles size 113400
 (List(0, 1, 2, 3, 4, 5, 6, 7),0) 20160 17640 1231 13877
 (List(0, 5, 6, 4, 1, 3, 8, 9),60000) 20160 17640 1214 13842
 Max score when n=8 and d=6 is (List(1, 8, 2, 3, 4, 6, 7, 9),23209) with the perms
 Min score when n=8 and d=6 is (List(0, 1, 4, 8, 2, 5, 9, 6),8265) with the perms
 ((List(0, 1, 4, 8, 2, 5, 9, 6),8265),(List(1, 8, 2, 3, 4, 6, 7, 9),23209))
 */


}