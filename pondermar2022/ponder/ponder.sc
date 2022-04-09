import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object ponder {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val elapsedns = t1 - t0
    println("Elapsed time: " + elapsedns.toString.reverse.grouped(3).mkString(",").reverse + "ns")
    result
  }                                               //> time: [R](block: => R)R

  def solve(numDigits: Int, quarterToTake: Int) = {
    //val numDigits = 5
    def isPrime(n: Int) = Iterator.from(2).takeWhile(x => x * x <= n).forall(n % _ != 0)
    println("Generating primes ")
    val allnDigitPrimes = time((BigInt(10).pow(numDigits - 1) until BigInt(10).pow(numDigits).toInt) filter (x => x.isProbablePrime(10))) // time ((scala.math.pow(10, numDigits-1).toInt until scala.math.pow(10, numDigits).toInt) filter isPrime )
    println("Converting primes ")
    val allnDigitPrimesIndexedSeq = time(allnDigitPrimes.map(_.toString.toIndexedSeq))
    println("allnDigitPrimes.size " + allnDigitPrimes.size)
    println("Generating primes by Bigints ")
    //val allnDigitPrimesByProbability = time((BigInt(10).pow(numDigits - 1) until BigInt(10).pow(numDigits).toInt) filter (x => x.isProbablePrime(10)))
   // println(allnDigitPrimesByProbability.size)
 //    println("ensuring prime lists are identical " + (allnDigitPrimes == allnDigitPrimesByProbability.map(_.toInt)))

    def pattern(p: Int, q: Int) = {
      (p.toString zip q.toString) map (_ match {
        case (pi, qi) if pi == qi => "Green"
        case (pi, qi) if q.toString.toCharArray.contains(pi) => "Yellow"
        case _ => "Grey"
      })
    }

    def patternFromIndexedSeqs(p: IndexedSeq[Char], q: IndexedSeq[Char]) = {
      (p zip q) map (_ match {
        case (pi, qi) if pi == qi => "Green"
        case (pi, qi) if q.toString.toCharArray.contains(pi) => "Yellow"
        case _ => "Grey"
      })
    }

 val powersOf3= (0 until numDigits).map( x => scala.math.pow(3,x).toInt)

 def patternFromIndexedSeqsToInt(p: IndexedSeq[Char], q: IndexedSeq[Char]) = {
  (p zip q).zipWithIndex.map (_ match {
    case (s, i) if s._1 == s._2     => 2 * powersOf3(i)
    case (s, i) if q.contains(s._1) => /*1 * */ powersOf3(i)
    case _ => 0
   }).sum
  }

    pattern(1377, 3751)
    pattern(3637, 4733)

    // Let f(p, q) be the number of remaining possible solutions after the first guess, given that p was the guess and q is the solution.
    /*  def f(p: Int, q: Int) = {
   val targetPatternToMatch = pattern(p,q)
   allnDigitPrimes.count(q2 => pattern(p,q2) == targetPatternToMatch)
  }*/
    // So in this case, f(3637, 4733)=11 .
    //f(3637, 4733)

    /*  val groups = allnDigitPrimesIndexedSeq.map{p =>
   allnDigitPrimesIndexedSeq.groupBy{q => patternFromIndexedSeqs(p,q)}//.groupBy(identity)
  }*/
    // Now we have the identity that
    /// f(p,q) == groups( pattern(p,q) ).size
    def f2(p: IndexedSeq[Char], q: IndexedSeq[Char]) = //groups(allnDigitPrimesIndexedSeq.indexOf(p))( patternFromIndexedSeqs(p, q) ).size
      allnDigitPrimesIndexedSeq.groupBy { q => patternFromIndexedSeqs(p, q) }(patternFromIndexedSeqs(p, q)).size

    //f2(3,7)
    //f2(3637, 4733)

    // we wish to minimize E[p]\triangleq\sum_{q\in P}\frac{f(p,q)}{|P|} where P is the set of n -digit primes.
    // E[p]= sum {over q in P}  f(p,q) / |P|   where P is the set of n -digit primes.
    // def E(p: IndexedSeq[Char]) = 1.0 / allnDigitPrimesIndexedSeq.size * allnDigitPrimesIndexedSeq.toIterator.map(q => f2(p, q)).sum

    // For an n digit number, we have 3^n (minus n maybe) possible patterns
    // That's only 2187 for n=7. and only 243 for n=5.
    // Say we name those pattern results 1 to 243 just for this comment,...
    // in code the patterns will be grouped by the raw form ("Grey","Green","Yellow","Grey","Grey") for example
    // So for a given pi, we can generate an array of results of pattern results pattern(pi,q) (one row of the matrix)
    // Vector(5, 243, 135, 5, ....)
    // We are interested in how often the patterns come up. We can see that the pattern 5 comes up twices already...
    // Our f(pi, q) function is really just asking how many times the pattern(pi,q) occurs. Obviously it'll be at least once.
    // But if we are summing the f function, we're better off grouping the patterns results.
    // 5 -> occurs at index 0, 3...
    // 243 -> occurs at index 1...
    // So this will easily give us f... Just look for the index, and then return the size.
    // We know that the indices sets will be disjoint and complete. They are disjoint from the definition of groupBy pattern. They are complete because we went through the whole array
    // We can go one step further since we summing f over all the values of q.
    // Say for the case of q=index 0, it gives pattern 5. And we know that that group occurs 2 times. So for each of the 2 times that we get to that q
    // we add 2. So we're adding 2, 2 times. So we can shortcut this calculation by adding the squares of the sizes of the sets!

    // We have an opportunity to optimize be checking the running sum of squares against the lowest found so far. ***

    def E2(p: IndexedSeq[Char]) =
      {
        val groupsOfResultsForP = allnDigitPrimesIndexedSeq.groupBy { q => patternFromIndexedSeqsToInt(p, q) }/*.view*/.mapValues(_.size)
        (1.0 / allnDigitPrimesIndexedSeq.size) * groupsOfResultsForP.map(x => x._2.toLong * x._2.toLong).sum
      }

     // How about short circuiting

  @tailrec
  def E3(p: IndexedSeq[Char], qList: List[ (IndexedSeq[Char], Int) ], minSoFar: Int, buckets: Array[Int], sumSoFar: Long): Option[Int] =
  {
   qList match {
   case (q, i) :: tail => {
//    if (i % 100 == 0 && buckets.map(b => b*b).sum > minSoFar)
    if(sumSoFar > minSoFar)
     None
    else
    {
     val patternNumber = patternFromIndexedSeqsToInt(p, q)
     /*
     println("patternNumber " + patternNumber)
     println("buckets.size " + buckets.size)
     buckets.foreach(println)
     */
     val oldBucket = buckets(patternNumber)
     buckets(patternNumber) = buckets(patternNumber) + 1
     E3(p, tail, minSoFar, buckets, sumSoFar + 2*oldBucket + 1)
    }
   }
   // We got through the whole list and it always stayed below minSoFar
   case Nil => {
    // The following line has an overflow bug! b*b overflows and wraps around to a negative number.
    val thisResult = buckets.map(b => b*b).sum
    if (thisResult < minSoFar)
     Some( thisResult  )
    else
     None
    }
   }
  }
  
  val allnDigitPrimesListWithIndex = allnDigitPrimesIndexedSeq.sortBy(x => x.toSet.size).toList.zipWithIndex
  @tailrec
  def iterateOverE3(pList: List[(IndexedSeq[Char], Int)], minSoFar: Int, minElementSoFar: IndexedSeq[Char]): (IndexedSeq[Char], Int) = {
   pList match {
   case (p, i) :: tail => {
    if (i%100 == 0)
     println("here 1 p " + p + " i " + i + " minSoFar " + minSoFar + " minSoFarElement " + minElementSoFar)
    E3(p, allnDigitPrimesListWithIndex, minSoFar, Array.fill(scala.math.pow(3,numDigits).toInt)(0), 0) match{
     case Some(min) => iterateOverE3(tail, min, p)
     case None => iterateOverE3(tail, minSoFar, minElementSoFar)
    }
   }
   case Nil => (minElementSoFar, minSoFar)
  }
 }
  // Sorting by least repeating to most repeating. The thought being that numbers like 1234567 should have lower scores
  // than 1000003.
//  val minP2 = iterateOverE3(allnDigitPrimesIndexedSeq.sortBy(x => - x.toSet.size).toList.zipWithIndex, scala.Int.MaxValue, IndexedSeq())
  val minP2 = iterateOverE3(allnDigitPrimesIndexedSeq.sortBy(x => - x.toSet.size).toList.zipWithIndex.drop(168800), 729641181, "1584269".toIndexedSeq)
   println("Solution minP2 " + minP2 + " minE= " + minP2._2*1.0/allnDigitPrimesIndexedSeq.size)


  /*
   val oneQuatreSize = allnDigitPrimesIndexedSeq.size /4 +1
   
    println("taking quartre " + quarterToTake)
    val minP = allnDigitPrimesIndexedSeq.zipWithIndex.drop(quarterToTake * oneQuatreSize).takeWhile{ case( _,i ) => i < (1+ quarterToTake) * oneQuatreSize}.minBy {
      case (p, i) =>
        if (i % 100 == 0)
          println("p " + p + " i " + i)
        E2(p)
    }
    val minE = E2(minP._1)
    println("Solution E2 " + minP + " " + minE)
    minP
  */
  
/*
   val fa2: Future[(IndexedSeq[Char],Int)] = Future {
    val quarterToTake = 1
    println("taking quartre " + quarterToTake)
    val minP = allnDigitPrimesIndexedSeq.zipWithIndex.drop(quarterToTake * oneQuatreSize).takeWhile{ case( _,i ) => i < (1+ quarterToTake) * oneQuatreSize}.minBy {
      case (p, i) =>
        if (i % 100 == 0)
          println("p " + p + " i " + i)
        E2(p)
    }
    val minE = E2(minP._1)
    println("Solution E2 " + minP + " " + minE)
    minP
  }
  fa2 foreach { posts =>
   println(posts)
  }

   val f3: Future[(IndexedSeq[Char],Int)] = Future {
   val quarterToTake = 2
    println("taking quartre " + quarterToTake)
    val minP = allnDigitPrimesIndexedSeq.zipWithIndex.drop(quarterToTake * oneQuatreSize).takeWhile{ case( _,i ) => i < (1+ quarterToTake) * oneQuatreSize}.minBy {
      case (p, i) =>
        if (i % 100 == 0)
          println("p " + p + " i " + i)
        E2(p)
    }
    val minE = E2(minP._1)
    println("Solution E2 " + minP + " " + minE)
    minP
  }
  f3 foreach { posts =>
   println(posts)
  }
  
   val f4: Future[(IndexedSeq[Char],Int)] = Future {
   val quarterToTake = 3
    println("taking quartre " + quarterToTake)
    val minP = allnDigitPrimesIndexedSeq.zipWithIndex.drop(quarterToTake * oneQuatreSize).takeWhile{ case( _,i ) => i < (1+ quarterToTake) * oneQuatreSize}.minBy {
      case (p, i) =>
        if (i % 100 == 0)
          println("p " + p + " i " + i)
        E2(p)
    }
    val minE = E2(minP._1)
    println("Solution E2 " + minP + " " + minE)
    minP
  }
  f4 foreach { posts =>
   println(posts)
  }*/
    // Testing numDigits = 2
    // Solution 17 4.238095238095238 with f2
    // Solution 17 4.238095238095238 with f!!!

    // Testing numDigits = 3
    // Solution 179 13.545454545454545 with f
    // Solution 179 13.54545454ge5454545 with f2

    // Testing numDigits = 4
    // Solution E2 1237 41.46748350612629

    // Testing numDigits = 5 ****
    // 17923 121.5416716489298
// Testing numDigits = 6
// Done. minSoFar 26338294 minSoFar/size 382.23513191884598 . minElementSoFarIndex 124679
// 2m17.167s
    // Testing numDigits = 7 Bonus
    //Done. minSoFar 729641181/ minSoFar/size 1244.94938583574617 . minElementSoFarIndex 1584269


  }                                               //> solve: (numDigits: Int, quarterToTake: Int)Unit
  def main(args: Array[String]) = {
   println("args.size" + args.size)
   args.foreach(println)
   println("args(0)" + args(0))
    time(solve(2, args(0).toInt))
    time(solve(3, args(0).toInt))
    time(solve(4, args(0).toInt))
    time(solve(5, args(0).toInt))
    //time(solve(6))
    time(solve(7, args(0).toInt))
  }                                               //> main: (args: Array[String])Unit

}