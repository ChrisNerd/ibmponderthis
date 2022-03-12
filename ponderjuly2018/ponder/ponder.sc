import annotation.tailrec
import collection.parallel.mutable.ParSeq
object ponder {

// def main(args: Array[String]) = {

 val factorizeCache = collection.mutable.Map[Int, List[Int]]()
                                                  //> factorizeCache  : scala.collection.mutable.Map[Int,List[Int]] = Map()
 def factorize(n: Int): List[Int] = {
  @tailrec
  def factors(tuple: (Int, Int, List[Int], Int)): List[Int] = {
   tuple match {
    case (1, _, acc, _)                 => acc
    case (n, k, acc, _) if (n % k == 0) => factors((n / k, k, acc ++ ParSeq(k), Math.sqrt(n / k).toInt))
    case (n, k, acc, sqr) if (k < sqr)  => factors(n, k + 1, acc, sqr)
    case (n, k, acc, sqr) if (k >= sqr) => factors((1, k, acc ++ ParSeq(n), 0))
   }
  }
  factorizeCache.getOrElseUpdate(n,
   factors((n, 2, List[Int](), Math.sqrt(n).toInt)))
 }                                                //> factorize: (n: Int)List[Int]
 factorize(24)                                    //> res0: List[Int] = List(2, 2, 2, 3)
 factorize(17)                                    //> res1: List[Int] = List(17)
 def genOthers(t: List[Int]): IndexedSeq[List[Int]] =
 {
	val factorsOfP = t.flatMap(factorize).sorted
  for {
   a <- 1 to t.sum/3
 if (t.product % a == 0)
 //  if (factorsOfP.intersect(factorize(a1)) == factorize(a1))
   b <- a to (t.sum-a)/2
   if ( (t.product/a) % b == 0 && t.product/a == b * (t.sum - a-b))
   //if (factorsOfP.intersect(factorize(b1)) == factorize(b1))
//   val c1 = t.sum - a1 -b1
 //  if (a1*b1*c1 == t.product)
   }
   yield (List(a,b,t.sum -a -b))
 }                                                //> genOthers: (t: List[Int])IndexedSeq[List[Int]]
  
 def isObscure(t: List[Int]) = {
  genOthers(t).distinct.size > 1
 }                                                //> isObscure: (t: List[Int])Boolean
  
 isObscure(List(2,2,6))                           //> res2: Boolean = false
 isObscure(List(2,8,9))                           //> res3: Boolean = true
 val t=(2,8,9)                                    //> t  : (Int, Int, Int) = (2,8,9)
 genOthers(List(15,22,48))                        //> res4: IndexedSeq[List[Int]] = Vector(List(12, 33, 40), List(15, 22, 48))
 genOthers(List(16,23,49))                        //> res5: IndexedSeq[List[Int]] = Vector(List(14, 28, 46), List(16, 23, 49))
 genOthers(List(17,24,50))                        //> res6: IndexedSeq[List[Int]] = Vector(List(17, 24, 50), List(20, 20, 51))
 genOthers(List(18,25,51))                        //> res7: IndexedSeq[List[Int]] = Vector(List(15, 34, 45), List(17, 27, 50), Li
                                                  //| st(18, 25, 51))

 val tot = 600                                    //> tot  : Int = 600
 val allSolutions = (for {
  a <- Iterator.range(1, tot/3)
  b <- a to (tot - a)/2
  c <- b to tot-a-b
 } yield List(a,b,c)
 ).withFilter{x =>
  (0 to 5).forall(year => isObscure(x.map(_+year))) || x(0)%100==0 && x(1)==x(0) && x(2)==x(0)
  }.foreach(println)                              //> List(100, 100, 100)
                                                  //| allSolutions  : Unit = ()
  
/*
 def efficientFind(firstInterval: Int, secondInterval: Int) = {
  var youngest = 1
  var streak = 0
//  val lengthOfStreak = 4
   val lengthOfStreak = 6 // For Bonus points!!!
  
  while (youngest < 1000){
   val t= List(youngest, youngest+firstInterval, youngest+firstInterval+secondInterval)
   if (isObscure(t))
   {
    youngest-=1
    streak+=1
   }
   else
   {
    youngest+=lengthOfStreak
    streak=0
   }
   if (streak==lengthOfStreak)
   {
    println(t)
    youngest+=lengthOfStreak+1
   }
  }
 }
 
 efficientFind(7,26)
 
 val allIntervals = for {
  firstInterval <- 0 to 200
  secondInterval <- 0 to 200
 } yield efficientFind(firstInterval, secondInterval)
 
 */
 //}
}