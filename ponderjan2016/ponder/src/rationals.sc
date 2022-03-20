object rationals {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def remainders(S1: List[Double], S2: List[Double]): Set[Int] =
    {
      val products = (for {
        s1 <- S1
        s2 <- S2
      } yield (s1 * s2)).toSet
// How about iterating through the rationals that i*j = n where n < 57
// i and j can each take 57^2 values.

//println(products.toList.sorted)
      val coveringSet = (products map (x => if (x== x.toInt)
      Set(x , (x-1) , (x+1))
      else
      Set(Math.floor(x), Math.ceil(x)))).flatten
  //    println(coveringSet.toList.sorted)
      
      (1 to 65).toSet -- (coveringSet map (_.toInt))
    }                                             //> remainders: (S1: List[Double], S2: List[Double])Set[Int]

  def numRemainders(S1: List[Double], S2: List[Double]) =
    {
      remainders(S1, S2).size
    }                                             //> numRemainders: (S1: List[Double], S2: List[Double])Int

  def f(S1: Set[Int], S2: Set[Int]): Double = {

    val products = (for {
      s1 <- S1
      s2 <- S2
    } yield (s1 * s2)).toList.sorted

    val pairsOfAdjacent = products.sliding(2).toList

    /*
######################################
# CHANGE the constant on the next line!!!
######################################
*/
    val penalty1a = pairsOfAdjacent filter (x => x(1) < 57 && x(1) - x(0) > 2)
    val penalty1b = penalty1a map (x => (x(1) - x(0) - 2) * (1000 - 5 * x(1)))
    val penalty1c = penalty1b.sum

    val k = products min
    val penalty2 = if (k > 2)
      k - 2
    else
      0
    (products max) - penalty1c - penalty2

    // Hmmm, do we only have to cover the integers?  Looks like it from the N=17 example
    // So maybe our objective function can just be the number of integers not covered, as in ponder.sc

  }                                               //> f: (S1: Set[Int], S2: Set[Int])Double

  f(Set(2, 2, 3), Set(2, 5, 6))                   //> res0: Double = -3719.0

  def listOfRationals(minel: Double, maxel: Int, n: Int, k: Int): Stream[List[Double]] = {
    if (k == n)
      Stream(List())
    else {
      val a = for {
        i <- (minel + 0.5) to (minel+4) by 0.5 //(2 * minel + 1).toInt to (2*minel + 5).toInt map (_ / 2.0)
        row <- listOfRationals(i, maxel, n, k + 1)
      } yield i :: row
      a.toStream
    }
  }                                               //> listOfRationals: (minel: Double, maxel: Int, n: Int, k: Int)Stream[List[Dou
                                                  //| ble]]
val S1Stream = listOfRationals(0,15,6,0)          //> S1Stream  : Stream[List[Double]] = Stream(List(0.5, 1.0, 1.5, 2.0, 2.5, 3.0
                                                  //| ), ?)
val S2Stream = listOfRationals(1,15,6,0)          //> S2Stream  : Stream[List[Double]] = Stream(List(1.5, 2.0, 2.5, 3.0, 3.5, 4.0
                                                  //| ), ?)|

for {
    S1 <- S1Stream
    S2 <- S2Stream
  } {
  
    if (numRemainders(S1, S2) == 0) {
      println("S1= ",S1)
      println("S2= ",S2)
    }
   }
  
  // This can now take in n as a Double, i.e. n=16.25 has factors like (2.5,6.5)
    def factorsRational(n: Double): List[(Double, Double)] =
    {
      (/*0.5*/ 1.0 to n by 0.5).toList filter (x => x * Math.floor(2*n / x) == 2*n) map (x => (x, n / x))
    }

  factorsRational(6)

  def covers(n: Int): List[(Double, Double)] =
    {
      if (n > 1)
      //  (factorsRational(n - 1) ::: factorsRational(n) ::: factorsRational(n + 1) )
(for (i <- -1.0 to 1.0 by 0.25)
yield factorsRational(n+i)).flatten.toList. filterNot (_ == List())
      else
       //factorsRational(n) ::: factorsRational(n + 1)
     (for (i <- 0.0 to 1.0 by 0.25)
yield factorsRational(n+i)).flatten.toList. filterNot (_ == List())
    }

  covers(3)
val a = for (i <- -1.0 to 1.0 by 0.25)
yield factorsRational(5.0+i)
               a. flatten

1.0 to 4.0
//  for(x <- 1.0 to 4.0) yield factorsRational(x)
  
  
  //(1.0 to 4.0) map covers

  def r(c: IndexedSeq[List[(Double, Double)]], S1: Set[Double], S2: Set[Double]): Boolean = {
    if (S1.size > 6 || S2.size > 6)
      false
    else if (c isEmpty) {
      println("Solution found!")
      println(S1.toList.sorted)
      println(S2.toList.sorted)
      true
    } else {
      // 4 cases
      // s1 is/isn't in S1
      // s2 is/isn't in S2
    
    // Start with the ones that don't add anything to the sets
//    val cParted = c partition(x => x exists(y => S1.contains(y._1) && S2.contains(y._2)))
    val cParted = c filterNot(x => x exists(y => S1.contains(y._1) && S2.contains(y._2)))
    
//      val cSorted = cParted._2 sortWith (_.length < _.length)
      
     val cParted2 = cParted filter (x => x exists(y => S1.contains(y._1)))
     val cParted3 = cParted filter (x => x exists(y => S2.contains(y._2)))
     val cParted4 = cParted filter (x => x forall(y => !S1.contains(y._1) && !S2.contains(y._2)))
      
      val cSorted = (cParted2 sortWith (_.length < _.length)) ++
      (cParted3 sortWith (_.length < _.length)) ++
      (cParted4 sortWith (_.length < _.length))
      /*
      println("c",c)
      println("cParted",cParted)
      println("cParted2",cParted2)
      println("cParted3",cParted3)
      println("cParted4",cParted4)
      println("c",c.size)
      println("cParted",cParted.size)
      println("cParted2",cParted2.size)
      println("cParted3",cParted3.size)
      println("cParted4",cParted4.size)
      */
      if (cSorted.isEmpty)
      r(cSorted,S1,S2)
      else
      {
 //     c.head exists (ses => r(c.tail, S1 + ses._1, S2 + ses._2))
  //   cSorted.head exists (ses => r(cSorted.tail, S1 + ses._1, S2 + ses._2))
//  val cSorted2 = cSorted
//true
//     (cSorted.head filter (ses => r(cParted /*cSorted.tail*/, S1 + ses._1, S2 + ses._2))).size > 0
val candidatePairsSorted = cParted.flatten.groupBy(identity)//.mapValues(_.size)//.toList.sortWith(_._2 > _._2)//.map(_._1)
println(candidatePairsSorted)
true
// (candidatePairsSorted filter ( ses => r(cParted, S1 + ses._1, S2 + ses._2))).size > 0
     }
    }
  }

covers(52).length

  val b = (1 to 3) map covers
  
  b.flatten.groupBy(identity).mapValues(_.size).toList.sortWith(_._2 > _._2)
  val d = (0.5 to 5.0 by 0.5) map (c => (c,b.count(p=> p.exists( x => x._1==c))))
  
  
  d.sortWith(_._2 > _._2)
  val S1: Set[Int] = Set(1, 2,3)
  val S2: Set[Int] = Set(2,3,4)
  //  val cParted =  (1 to 15) map covers partition(x => x exists(y => S1.contains(y._1) && S2.contains(y._2)))
  // (1 to 60) map covers sortWith (_.length < _.length) map (x => x.head._2 + 1)
                                                  
  covers(55)
  r((1 to 45) map covers, Set(), Set())
  // numRemainders(List(1.0, 3.5, 6.0, 7.5, 10.0, 10.5),List(1.5, 4.0, 4.5, 5.0, 5.5, 6.0))

 

}