object ponder {
 // Not the Sieve of Eratosthenes, this is trial division and runs out of heap space for 100000
 def primeStream(s: Stream[Int]): Stream[Int] =
    Stream.cons(s.head, primeStream(s.tail filter { n => n % s.head != 0 }))
                                                  //> primeStream: (s: Stream[Int])Stream[Int]
 // Precompute a giant list of primes so they can be iterated over.
 val primes = primeStream(Stream.from(2)).takeWhile(_ <= 100.toLong).toList.toStream
                                                  //> primes  : scala.collection.immutable.Stream[Int] = Stream(2, ?)
 primes take 6 foreach println                    //> 2
                                                  //| 3
                                                  //| 5
                                                  //| 7
                                                  //| 11
                                                  //| 13
 // Find all the prime factors of a, with repetition. If a = 12, return List(2,2,3)
 def primeDecomposition(a: Long, startingFrom: Stream[Int]): List[Long] = {
  startingFrom.takeWhile(prime => prime <= math.sqrt(a)).find(prime =>  a % prime == 0) match {
   case Some(prime) => prime :: primeDecomposition( a / prime, Stream.cons(prime, startingFrom))  // Prepend this found prime to the start of the stream in case it's repeated.
   case _ => List(a)
  }
 }                                                //> primeDecomposition: (a: Long, startingFrom: Stream[Int])List[Long]
 
 // To be used in conjunction with the output from primeDecomponsition.
 // factors takes in the counts of the primes. If a = 12, primeDecomposition returns List(2,2,3).
 // We get the counts by doing the .groupBy(identity).mapValues(_.size) which gives Map(2->2, 3->1)
 // factors takes in that map and outputs List(1,2,3,4,6,12) (don't worry about the order)
 // factors are 2^{0,1,2} * the factors of 3. The factors of 3 are just 3 and 1.
 // The base case in recursion is factors of 3 = 3^{0, 1} * factors of {empty Map}.
 // factors of empty map is the multiplicative identity: 1.
 def factors( pD: Map[Long,Int]): Iterable[Int] = pD.size match {
  case 0 =>  Iterable(1)
  case _ =>
  {
   val (base,exponent) = pD.head
   for {
    f <- factors ( pD - base )
    e <- 0 to exponent
   } yield (scala.math.pow(base,e) * f ).toInt
  }
 }                                                //> factors: (pD: Map[Long,Int])Iterable[Int]

 primeDecomposition(12, primes)                   //> res0: List[Long] = List(2, 2, 3)
 primeDecomposition(144, primes)                  //> res1: List[Long] = List(2, 2, 2, 2, 3, 3)
 val primeDecompositionval = primeDecomposition(12, primes)
                                                  //> primeDecompositionval  : List[Long] = List(2, 2, 3)
 val mapOfDecomposition = primeDecompositionval.groupBy(identity).mapValues(_.size)
                                                  //> mapOfDecomposition  : scala.collection.immutable.Map[Long,Int] = Map(2 -> 2
                                                  //| , 3 -> 1)
 factors( mapOfDecomposition )                    //> res2: Iterable[Int] = List(1, 2, 4, 3, 6, 12)


 // Given an integer a, returns all (nonnegative) integer solutions for b where
 // a+b^2 is a square.
 // bsFromA(125) = List(62,10) meaning
 // 125+62^2 ( = 3969 = 63^2)
 // 125+10^2 ( = 225 = 15^2 )
 // 62 and 10 are the only solutions for b.
 
 /* The math is as follows.
 // a+b^2 = n^2
 // a = n^2-b^2
  = (n-b)(n+b) from the difference of squares.
  which means that a is expressed as a factored pair.
  We want to limit our answers to nonnegative b since we are squaring b anyway.
  So n-b is the smaller factor, and n+b is the larger.
  Find all the factors q of a, where q <= sqrt(a)
  The pairs of factors q * (a/q), correspond to (n-b)(n+b), where
  q <= a/q and
  n-b <= n+b.
  
  Solving the simultaneous equations
  q = n-b
  a/q = n+b
  by subtracting the first from the second.
  a/q - q = 2b.
  (a/q - q) /2 = b
  Now we see that a/q - q must be even, which means that they must have the same parity, so we limit ourselves to factors q such that
  (a/q+q) %2 == 0 ... ( or (a/q-q) %2 == 0)
  If a is negative there's only a sign change.
 */
 def bsFromA(a: Long, doPositive: Int) = {
  factors(primeDecomposition(a, primes).groupBy(identity).mapValues(_.size))
  .filter(q => (a/q >= q) && (a/q + q) % 2 == 0 ).map(q => (a/q- doPositive * q)/2)
 }                                                //> bsFromA: (a: Long, doPositive: Int)Iterable[Long]
 bsFromA(125, 1)                                  //> res3: Iterable[Long] = List(62, 10)
 

 // So we're given 4 b's {b1, b2, b3, b4}, that are known to work with a given 3 a's (we're not given the as). We want to generate the
 // complete list of a's that will work with those 4 b's.
 
 // Given the 4 b's in:
 // a = n1^2 - b1^2
 //   = n2^2 - b2^2
 //   = n3^2 - b3^2
 //   = n4^2 - b4^2
 // Find all a's.
 // We take the equations pairwise (4 choose 2 = 6) of them.
 // a = n1^2 - b1^2 = n2^2 - b2^2 is 1 equation. We put both bs on one side.
 // n1^2 - n2^2 = b1^2 - b2^2. Now we've reduced the problem to a known problem (namely, b1^2 - b2^2 is a constant, we can solve for all solutions n1 and n2).
 // Our call to bsFromA returns all n2 values (which we call n2s)
 // Since we are interested in the set of a values a (= n2^2 - b2^2), and we now have a set of n2's, we carry out that equation to get our set of a's (for this pair).
 // We repeat this process, calculating a new set of a's for each of the 6 pairs.
 // The a's that are common to all six pairings solve all the original 4 equations. Finding common elements in the 6 sets is done by the sexy .reduce(_ intersect _ ), which is the intersection of all sets.
 def generateAllAsFromSetOf4Bs(bSet: scala.collection.immutable.Set[Long]): scala.collection.immutable.Set[Long] = {
  val bPairs = bSet.subsets(2) // There should be 6 pairs (4 choose 2), but this is extendable to more than 4, and also could be used if b is of size less than 4 too (except the reduce will crash if it sees an empty set)
  val asFromBPairs = bPairs.map{ bs =>
   val b1SquaredMinusb2Squared = bs.max*bs.max - bs.min*bs.min
   val n2s = bsFromA(b1SquaredMinusb2Squared , if (b1SquaredMinusb2Squared >=0 ) 1 else -1).toSet
   n2s.map(n2 => n2*n2 - bs.min*bs.min)
  }
  asFromBPairs.reduce(_ intersect _)
 }                                                //> generateAllAsFromSetOf4Bs: (bSet: scala.collection.immutable.Set[Long])scal
                                                  //| a.collection.immutable.Set[Long]
 
 val positives = Stream.from(1)                   //> positives  : scala.collection.immutable.Stream[Int] = Stream(1, ?)
 val negatives = Stream.from(-1,-1)               //> negatives  : scala.collection.immutable.Stream[Int] = Stream(-1, ?)
 
 val as = (positives zip negatives).map(x=> List(x._1,x._2)).flatten
                                                  //> as  : scala.collection.immutable.Stream[Int] = Stream(1, ?)

 /*val all3 = as.map{ a =>
  val bs = bsFromA(a, if (a >= 0) 1 else -1)
  val allAs = bs.toSet.subsets(4).map(generateAllAsFromSetOf4Bs)
  (a, bs, allAs.toList )
 }
 */
 val cheat = (math.pow(2,11)*math.pow(3,2)).toLong//> cheat  : Long = 18432
 val solUsingStream = for {
  a <- as
  val bs = bsFromA(a * cheat, if (a >=0) 1 else -1)
  allAs <- bs.toSet.subsets(4).map(generateAllAsFromSetOf4Bs)
  if (allAs.size >=5 || a%1/*00000*/ == 0)
 } yield (a, bs, allAs)                           //> solUsingStream  : scala.collection.immutable.Stream[(Int, Iterable[Long], s
                                                  //| cala.collection.immutable.Set[Long])] = Stream((1,List(4607, 2302, 1148, 56
                                                  //| 8, 272, 112, 8, 1533, 762, 372, 168, 48, 503, 238, 92),Set(0, 18432)), ?)
 
 // Gah! This takes too long (nothing after like 3 hours, even with the cheat
 //solUsingStream.foreach(println)
 
/* val solsUsingStream = all3.filter{ _._3.size >= 5}
 solsUsingStream.foreach(println)
*/
 
 generateAllAsFromSetOf4Bs(Set(1174, 589, 446, 346))
                                                  //> res4: scala.collection.immutable.Set[Long] = Set(0, -119392, 144480, -64960
                                                  //| )

// try as as a list of highly composite numbers, then take subsets(4)
 val highlyCompositions = List(/*1, 2, 4, 6, 12, 24,*/ 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560, 10080, 15120, 20160, 25200, 27720, 45360, 50400, 55440, 83160, 110880, 166320, 221760, 277200, 332640, 498960, 554400, 665280, 720720, 1081080, 1441440, 2162160 )
                                                  //> highlyCompositions  : List[Int] = List(36, 48, 60, 120, 180, 240, 360, 720,
                                                  //|  840, 1260, 1680, 2520, 5040, 7560, 10080, 15120, 20160, 25200, 27720, 4536
                                                  //| 0, 50400, 55440, 83160, 110880, 166320, 221760, 277200, 332640, 498960, 554
                                                  //| 400, 665280, 720720, 1081080, 1441440, 2162160)
 
 val bs = highlyCompositions.map(h => bsFromA(h, 1))
                                                  //> bs  : List[Iterable[Long]] = List(List(8, 0), List(11, 4, 1), List(14, 2), 
                                                  //| List(29, 13, 1, 7), List(44, 4, 12), List(59, 28, 11, 7, 17, 4), List(89, 4
                                                  //| 3, 13, 27, 9, 1), List(179, 88, 41, 31, 8, 57, 24, 3, 11), List(209, 103, 3
                                                  //| 7, 11, 23, 1, 67, 29), List(314, 58, 38, 102, 6, 26), List(419, 208, 101, 7
                                                  //| 9, 32, 1, 53, 16, 137, 64, 23, 13), List(629, 313, 121, 53, 83, 31, 207, 99
                                                  //| , 27, 9, 61, 17), List(1259, 628, 311, 247, 116, 43, 173, 76, 17, 1, 417, 2
                                                  //| 04, 93, 69, 12, 39, 131, 52), List(1889, 943, 373, 179, 263, 121, 19, 627, 
                                                  //| 309, 111, 33, 69, 3, 201, 87, 43), List(2519, 1258, 626, 307, 499, 242, 106
                                                  //| , 23, 353, 166, 62, 37, 837, 414, 198, 81, 153, 54, 99, 18, 271, 122, 34, 1
                                                  //| 1), List(3779, 1888, 941, 751, 368, 169, 533, 256, 107, 73, 1257, 624, 303,
                                                  //|  237, 96, 3, 159, 48, 411, 192, 69, 39, 113, 16), List(5039, 2518, 1256, 62
                                                  //| 2, 299, 1003, 494, 232, 86, 713, 346, 152, 34, 109, 2, 1677, 834, 408, 186,
                                                  //|  57, 321, 138, 24, 219,
                                                  //| Output exceeds cutoff limit.
 
 val astest = bs.map(b => generateAllAsFromSetOf4Bs(b.toSet) )
                                                  //> astest  : List[scala.collection.immutable.Set[Long]] = List(Set(225, 36, 0)
                                                  //| , Set(48, 0), Set(0, 480, 2205, 60, 165), Set(0, 120), Set(0, 180), Set(0, 
                                                  //| 240), Set(0, 360), Set(0, 720), Set(0, 840), Set(0, 1260), Set(), Set(), Se
                                                  //| t(), Set(), Set(), Set(), Set(), Set(), Set(), Set(), Set(), Set(), Set(), 
                                                  //| Set(), Set(), Set(), Set(), Set(), Set(), Set(), Set(), Set(), Set(), Set()
                                                  //| , Set())
 astest.filter(_.size>=5)                         //> res5: List[scala.collection.immutable.Set[Long]] = List(Set(0, 480, 2205, 6
                                                  //| 0, 165))
 // This finds the highly composite number of 60 as a solution. Since its bs (2,14) result in the following 5 pairs of squares
 // 480+2^2
 // 480+14^2
 // 2205+2^2
 // 2205+14^2 ... 60, 165 and 0.
                                                  
 // cheat 10
 //((-8370,-13158),104652,Set(3696, 10136, 4296, 4944))
 //Set(0, -12374460, -8570880, 107163648, -13473792)
 //(Bonus Solution Found!!,Set(0, -12374460, -8570880, 107163648, -13473792))
 
 // Verify the solution to the bonus question
 val ss = for {
  a <- List(0, -12374460, -8570880, 107163648, -13473792)
  b <- List(3696, 10136, 4296, 4944)}
  yield(math.sqrt(a+b*b))                         //> ss  : List[Double] = List(3696.0, 10136.0, 4296.0, 4944.0, 1134.0, 9506.0, 
                                                  //| 2466.0, 3474.0, 2256.0, 9704.0, 3144.0, 3984.0, 10992.0, 14488.0, 11208.0, 
                                                  //| 11472.0, 432.0, 9448.0, 2232.0, 3312.0)
 val bools = ss.map( s => s.toInt * s.toInt == s*s)
                                                  //> bools  : List[Boolean] = List(true, true, true, true, true, true, true, tru
                                                  //| e, true, true, true, true, true, true, true, true, true, true, true, true)
 bools.reduce(_ && _)                             //> res6: Boolean = true
 // This is old and O(sqrt(a)) so we've deprecated it. It's replaced with bsFromAList. We've tested that the two produce the same results.
/* def bsFromA(a: Long, doPositive: Int)/*: ParSet[Int]*/ = {
  for (q <- (1 to math.floor(math.sqrt(a)).toInt).toSet
   if (a%q == 0 && (a/q + q) % 2 == 0))
  yield ((a/q - doPositive * q)/2)//.toInt
 }

 bsFromAList(675, 1)
 bsFromA(675, -1)
 bsFromAList(675, -1)
*/
 // first import all necessary types from package `collection.mutable`
 import collection.mutable.{ HashMap, MultiMap, Set }

 // to create a `MultiMap` the easiest way is to mixin it into a normal
 // `Map` instance
 val mm = new HashMap[Int, Set[String]] with MultiMap[Int, String]
                                                  //> mm  : scala.collection.mutable.HashMap[Int,scala.collection.mutable.Set[Str
                                                  //| ing]] with scala.collection.mutable.MultiMap[Int,String] = Map()

 val bsAsAFunctionOfA = new HashMap[Long, Set[Long]] with MultiMap[Long, Long] // this is to replace as : Map[Long, Set[Long]]. The key is A, the value is Set[B]
                                                  //> bsAsAFunctionOfA  : scala.collection.mutable.HashMap[Long,scala.collection.
                                                  //| mutable.Set[Long]] with scala.collection.mutable.MultiMap[Long,Long] = Map(
                                                  //| )
 val asAsAFunctionOfB = new HashMap[Long, Set[Long]] with MultiMap[Long, Long]
                                                  //> asAsAFunctionOfB  : scala.collection.mutable.HashMap[Long,scala.collection.
                                                  //| mutable.Set[Long]] with scala.collection.mutable.MultiMap[Long,Long] = Map(
                                                  //| )

 val bsAsAFunctionOfPairOfAs = new HashMap[ (Long,Long) , Set[Long]] with MultiMap[(Long,Long), Long]
                                                  //> bsAsAFunctionOfPairOfAs  : scala.collection.mutable.HashMap[(Long, Long),sc
                                                  //| ala.collection.mutable.Set[Long]] with scala.collection.mutable.MultiMap[(L
                                                  //| ong, Long),Long] = Map()
 val possibleSolutionBs = bsAsAFunctionOfPairOfAs.getOrElse( (5,10) , Set()) intersect Set(5)
                                                  //> possibleSolutionBs  : scala.collection.mutable.Set[Long] = Set()

 val pairsOfAsAsAFunctionOfB = new HashMap[Long, Set[(Long,Long)]] with MultiMap[Long, (Long,Long)]
                                                  //> pairsOfAsAsAFunctionOfB  : scala.collection.mutable.HashMap[Long,scala.coll
                                                  //| ection.mutable.Set[(Long, Long)]] with scala.collection.mutable.MultiMap[Lo
                                                  //| ng,(Long, Long)] = Map()
 // to add key-value pairs to a multimap it is important to use
 // the method `addBinding` because standard methods like `+` will
 // overwrite the complete key-value pair instead of adding the
 // value to the existing key
 mm.addBinding(1, "a")                            //> res7: ponder.<refinement>.type = Map(1 -> Set(a))
 mm.addBinding(2, "b")                            //> res8: ponder.<refinement>.type = Map(2 -> Set(b), 1 -> Set(a))
 mm.addBinding(1, "c")                            //> res9: ponder.<refinement>.type = Map(2 -> Set(b), 1 -> Set(c, a))

 // mm now contains `Map(2 -> Set(b), 1 -> Set(c, a))`

 // to check if the multimap contains a value there is method
 // `entryExists`, which allows to traverse the including set
 mm.entryExists(1, _ == "a") == true              //> res10: Boolean = true
 mm.entryExists(1, _ == "b") == false             //> res11: Boolean = true
 mm.entryExists(2, _ == "b") == true              //> res12: Boolean = true

 // to remove a previous added value there is the method `removeBinding`
 mm.removeBinding(1, "a")                         //> res13: ponder.<refinement>.type = Map(2 -> Set(b), 1 -> Set(c))
 mm.entryExists(1, _ == "a") == false             //> res14: Boolean = true
 val bonus = 4                                    //> bonus  : Int = 4
 // bonus = 4 prints ((-64960,-119392),144480,Set(589, 1174, 446, 346))



 // The algorithm works as follows.
 // Guess at a (iterate over all a's, a={1,-1,2,-2,3,-3...}
 // Keep a running list (mutable map of a -> Set[b]) of b's that are associated with previously tried a's (i.e. b's that solve a+b^2 is a square)
 // Limit that list to sets of b that are at least size 4.
 // Keep a running list (mutable map of (a1,a2) -> Set[b]) of 'b's that are the solutions to pairs of a's. (i.e. b's that solve a1+b^2 is a square and a2+b^2 is another square. Same 4 b's in both equations)
 // Check to see if this a can form a triplet with an existing pair of as. (a1,a2,a) such that a1+b^2, a2+b^2, a+b^2 work for all 4 b's.
 // This check is done by iterating over our b's. For each b create a list of pairs of as such that the pair of a's has that b. We can then easily count if a pair of a's shows up at least 4 times.
 // If so, we've solved the case for 4 a's and 4 b's. (a=0 is a trivial a).
 // If not, we see if we can form a new pair (a1,a) that has 4 b's, and add it to the list.
 // We use mutable multimaps so we can do our checks in O(1) time provided we have lots of memory.
 // The wrong way to get the bonus question is to try to find a set of 5 b's, with 3 a's. There is no solution with the 3 a's being less than 80 million (which took 50 hours of computer time and 6 GB of memory).
 // The way I found the bonus question was to look at the factors of the 3 a's solution, and notice they factored with lots of 2s, e. g. 2, 2, 2, 2, 2, 2, 5, 7, 29.
 // This made me think that they were highly compositive numbers.
 // They had a very high greatest common factor.
 // And we could take a big cheat shortcut by considering only a's of the form 2^c * n, where c was something like 10. This let us efficiently find lots of 4 a's, 4 b's solutions.
 // The other big breakthrough was instead of trying to find 5 b's, we took the 4 existing b's that work for a triplet of a's, and generated the complete list of a's! (using generateAllAsFromSetOf4Bs)
 // After only a few triplets of a's, we find a set of b's that can generate 5 a's (including 0) that solve the bonus question.
 def tryNewA( ): Unit = {
  var a = 1
  var doPositive = true
  var foundSolution = false
  while (!foundSolution)
  {
	 val bs = bsFromA( cheat.toLong * a, if (doPositive) 1 else -1).toSet
   if (a%100000==0)
    println(a, doPositive, asAsAFunctionOfB.size, pairsOfAsAsAFunctionOfB.size, bs)

   if ( (bs - 0 ).size < bonus )
   {
    a+=1
    doPositive = true
   }
   else
   {
    bs.flatMap( b => pairsOfAsAsAFunctionOfB.get(b))
    // Now have to count all the unique APairs and see if there's any APair that has a count of 4
    // We have a Set[Set[Pair]], how can get some sort of output that looks like a1 -> 3, a2 -> 2, a3 ->5...
    // Covert to a Set[List[Pair]], then List[List[Pair]], then flatten to List[Pair], then .groupBy(identity).mapValues(_.size)
    // will give [Pair]->count
    .map(_.toList)
    .toList
    .flatten
    .groupBy(identity).mapValues(_.size)
    .find{ case ((a1,a2), count) => count >= bonus} match {
     case Some( ((a1,a2), count) ) =>  {
      val possibleSolutionBs = bs intersect bsAsAFunctionOfPairOfAs.getOrElse((a1,a2), Set())
      println((a1,a2), if (doPositive) a else -a, possibleSolutionBs)
      val allAs = generateAllAsFromSetOf4Bs(possibleSolutionBs)
      if (allAs.size > 5)
       {
       println("Bonus Solution Found!!", allAs)
       foundSolution = true
      }
      a=math.abs(a)+1
     }
     case None => {
      // Add any possible pairs
      bs.flatMap( b => asAsAFunctionOfB.get(b))
      .flatten  // Does the Union on the sets of a to get a list of condidate a's.
      .filter( possibleA => ( bsAsAFunctionOfA.getOrElse(possibleA, Set()) intersect bs).size >= bonus)
      .foreach{ possibleA =>
        val b = bsAsAFunctionOfA.getOrElse(possibleA, Set()) intersect bs
        b.foreach{ bInt =>
         bsAsAFunctionOfPairOfAs.addBinding( (possibleA, if (doPositive) a else -a) , bInt)
         pairsOfAsAsAFunctionOfB.addBinding(bInt, (possibleA, if (doPositive) a else -a))
       }
      }
      // Just add the plain old a and b's. It's important we do this last because if we did this before the other checks
      // we'd find a intersects a, as a false solution. Boo mutable data structures!
      bs.foreach{ b=>
       asAsAFunctionOfB.addBinding(b, if (doPositive) a else -a)
       bsAsAFunctionOfA.addBinding(if (doPositive) a else -a, b)
      }
      // do a= 1,-1,2,-2,3,-3...
      a = if (doPositive) a else a+1
      doPositive = !doPositive
      
      
      // Purely immutable way of doing this. Does not work with multimaps because they need to be mutable, (Booo!)
      //tryNewA( if (doPositive) a else a+1, !doPositive)
      //               as + ((if (doPositive) a else -a) -> bs),
      //		    pairsOfAs ++ as.mapValues(x => (x._1, if (doPositive) a else -a, x._2 intersect bs )).filter(_._3.size >= bonus))
	  	//    pairsOfAs ++ (bs.map(b=> bsMapOfAs.get(b)).reduce(_ union _)).mapValues(x => (x._1, if (doPositive) a else -a, x._2 intersect bs )).filter(_._3.size >= bonus))
		    
		  }
    }
   }
  }
 }                                                //> tryNewA: ()Unit
/* bsFromA(675,1)
 bsFromA(675,-1)
 bsFromA(270,1)
 bsFromA(960,1)
 bsFromA(1188,1)

 bsFromA(675,1).foreach( b=> asAsAFunctionOfB.addBinding(b, 675))
 */
 // Will print the solution -- (54432,119392,263872,Set(1122, 477, 18, 282))
 // The mutable version finds ((64961,-119392),144480,Set(589, 1174, 446, 346))
 // Hmmm, the mutuable one doesn't work, but it's related to the first solution. 54432+64961=119393, and 119392+144480=263872
 // AHHH, fonud the bug. Had an "a+1" where I meant "-a".
 // Now it outputs ((-64960,-119392),144480,Set(589, 1174, 446, 346))
 
 // ((73728,188416),335872,Set(128896, 137216, 8388574, 4456384, 1048304, 17825776, 61184, 16777199, 71303164, 523744, 26624, 277504, 1113856, 285212671, 8912864, 142606334, 35651576, 261056, 65536, 556544, 2228096, 1024, 24064, 2097016, 4194236))
 

tryNewA                                           //> ((741,-1044),-1265,Set(11568, 7032, 5008, 4848))
                                                  //| ((-485,-966),1729,Set(5136, 6204, 23376, 4456))
                                                  //| ((-221,1044),1785,Set(5496, 2064, 10704, 2416))
                                                  //| ((221,1265),2006,Set(1328, 5112, 10512, 432))
                                                  //| ((-481,485),2214,Set(23184, 3304, 4176, 5436))
                                                  //| ((481,966),2695,Set(22992, 2928, 1432, 4548))
                                                  //| ((-2030,-3731),4515,Set(14136, 28176, 10704, 8304))
                                                  //| ((-1680,-2090),4950,Set(11472, 20544, 6208, 7104))
                                                  //| ((2964,-4176),-5060,Set(14064, 10016, 23136, 9696))
                                                  //| ((-465,-731),5814,Set(3696, 10136, 4296, 4944))
                                                  //| ((-266,465),6279,Set(9704, 3984, 3144, 2256))
                                                  //| ((-1701,2030),6545,Set(8784, 27504, 5616, 12744))
                                                  //| ((-410,1680),6630,Set(19776, 4416, 2752, 10032))
                                                  //| ((-1940,-3864),6916,Set(10272, 46752, 12408, 8912))
                                                  //| ((-1653,-3003),6972,Set(9264, 26736, 11640, 7764))
                                                  //| ((410,2090),7040,Set(9648, 128, 3456, 19584))
                                                  //| ((-884,4176),7140,Set(4128, 4832, 10992, 21408))
                                                  //| ((884,5060),8024,Set(21024, 2656, 864, 10224))
                                                  //| ((-5865,-
                                                  //| Output exceeds cutoff limit.|

def main(args: Array[String]) = {
 println("Starting with bonus = ", bonus, " cheat ", cheat)
 tryNewA
}
 // n=[0, -64960,-119392,144480];b=[1174, 589, 446, 346]
/*b =

   1174    589    446    346

octave:34> b=b.^2;b=[b;ones(1,4)];n=[ones(1,4);n];sqrt(b'*n)
ans =

   1174   1146   1122   1234
    589    531    477    701
    446    366    282    586
    346    234     18    514*/
    
}