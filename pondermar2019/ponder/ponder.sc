import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap

object ponder {
 // Let N1 be the number of electrons in one coulomb of electric charge.
 // 1 C/e (coulomb per elementary charge)
 // Quantity[1, "Coulombs"/"ElementaryCharge"]
 // 6.2415091×10^18
 val N1 = BigDecimal(6.2415091*math.pow(10,18)).toBigInt
                                                  //> N1  : scala.math.BigInt = 6241509100000000000
 // Let N2 be energy (measured in electron volts) released in the fusion of 1,000 deuterium atoms and 1,000 tritium atoms
 // to form 1,000 He-4 atoms and 1,000 neutrons.
 // https://en.wikipedia.org/wiki/Nuclear_fusion#Criteria_and_candidates_for_terrestrial_reactions
 // (1) 	2 1D + 	3  1T 	→ 	4 2He ( 	3.5 MeV 	) 	+ 	n0 	( 	14.1 MeV 	)
 // Where (2 1) and (3 1) and (4 2) are (superscript subscript) for (mass number and proton count (atomic number)), so n0 should have (1 0).
 // So N2 = 1000*(3.5E6+14.1E6)
 // Using deuterium-tritium fuel, the resulting energy barrier is about 0.1 MeV. In comparison, the energy needed to remove an electron from hydrogen is 13.6 eV, about 7500 times less energy. The (intermediate) result of the fusion is an unstable 5He nucleus, which immediately ejects a neutron with 14.1 MeV. The recoil energy of the remaining 4He nucleus is 3.5 MeV, so the total energy liberated is 17.6 MeV.
 val N2 = BigDecimal(1000*(3.5*math.pow(10,6) + 14.1*math.pow(10,6))).toBigInt
                                                  //> N2  : scala.math.BigInt = 17600000000


 // we can also use the relation for di
 // d_{i+1}=d_{i-1}+-c_{i}^{2}d_{i}+2c_{i}a_{i}
 // It's probably best that we don't, since it requires two previous di values, but it keeps away int division (which always divides evenly anyway).
 // But this proves that the division /di is always a whole number.

  def confinuedFractionCoefficientsOfRootNplusKoverD(n: Int , ki: Int , di: Int): Stream[Int] = {
   val a1 = math.floor(math.sqrt(n)).toInt
   val ai1 = math.floor((a1+ki)/di).toInt
   ai1 #:: confinuedFractionCoefficientsOfRootNplusKoverD(n, -ki + ai1*di, (n - math.pow(ki -ai1*di,2).toInt)/di)
  }                                               //> confinuedFractionCoefficientsOfRootNplusKoverD: (n: Int, ki: Int, di: Int)S
                                                  //| tream[Int]
                  
 confinuedFractionCoefficientsOfRootNplusKoverD(14, 0, 1).take(10).foreach(println)
                                                  //> 3
                                                  //| 1
                                                  //| 2
                                                  //| 1
                                                  //| 6
                                                  //| 1
                                                  //| 2
                                                  //| 1
                                                  //| 6
                                                  //| 1

  //c × the previous denominator + the denominator before that
  /*de = cf *de.tail + de
 d0 = 0
 d1 = 1
 So d2 = c2*d1+d0

 c1 c2 c3  = cf // c1 is the first term, i.e. == a.
 But d starts at 0, so eventually we need to drop the first term.

 Use the following three streams
 c2 c3 c4  = cf.tail
 d0 d1 d2  = de(cf)
 d1 d2 d3  = de(cf).tail
 ========  To produce ... so the map will take (c2,(d1,d0)) => d2
 d2 d3 d4

 (de.tail).zip(de) // (d1,d0) (d2,d1)...
 cf.tail.zip((de.tail).zip(de)) // (c2,(d1,d0))  (c3,(d2,d1))...
 cf.tail.zip((de.tail).zip(de)).map{ n => n._1*n._2._1 + n._2._2  }
 Now we have the ingredients to calculate d2 = c2*d1+d0

 The last step is to drop the first term d0 because it is "virtual". cf starts at index 1 so we are consistent.

 This function has exponential run-time complexity. Do not use! Use the one with iterators, which is O(n), like it should be.
 def de(cf: Stream[Int]): Stream[BigInt] =
  BigInt(0) #:: BigInt(1) #:: cf.tail.zip((de(cf).tail).zip(de(cf))).map{ case (cf,(dminus2, dminus1))  => dminus2*cf + dminus1  }


 */

/*  def numeratorStream(cf: Stream[Int]): Stream[Int] =
  1         #:: cf.head   #::
  cf.tail.zip(
  (numeratorStream(cf).tail).zip(
  numeratorStream(cf))).map{ case (cf,(nminus2, nminus1))  => cf*nminus2 + nminus1  }
*/
  def f(starting: Iterator[BigInt], cf: Stream[Int]): Iterator[BigInt] = {
  val cfIt = cf.iterator
  cfIt.next
  starting ++ (
   for{
    lastTwo <- f(starting, cf).sliding(2)
   } yield cfIt.next * lastTwo(1) + lastTwo(0)
   )
  }                                               //> f: (starting: Iterator[BigInt], cf: Stream[Int])Iterator[BigInt]
 
/*
 def numeratorStream(cf: Stream[Int])   =f( Iterator(BigInt(1), BigInt(cf.head)), cf)
 def denominatorStream(cf: Stream[Int]) =f( Iterator(BigInt(0), BigInt(1))      , cf)
*/
 def numeratorStream(cf: Stream[Int]): Iterator[BigInt] = {
  val cfIt = cf.iterator
  cfIt.next
  Iterator(BigInt(1), BigInt(cf.head)) ++ (
   for{
    lastTwo <- numeratorStream(cf).sliding(2)
   } yield cfIt.next * lastTwo(1) + lastTwo(0)
   )
  }                                               //> numeratorStream: (cf: Stream[Int])Iterator[BigInt]

 def denominatorStream(cf: Stream[Int]): Iterator[BigInt] = {
  val cfIt = cf.iterator
  cfIt.next
  Iterator(BigInt(0), BigInt(1)) ++ (
   for{
    lastTwo <- denominatorStream(cf).sliding(2)
   } yield cfIt.next* lastTwo(1) + lastTwo(0)
   )
 }                                                //> denominatorStream: (cf: Stream[Int])Iterator[BigInt]
 
 denominatorStream(Stream(0,1)).take(3).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
 denominatorStream(Stream(0,1,3)).take(4).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 4
 denominatorStream(Stream(0,1,4)).take(4).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 5
 denominatorStream(Stream(0,1,5)).take(4).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 6
 denominatorStream(Stream(0,1,5,2)).take(5).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 6
                                                  //| 13
 denominatorStream(Stream(0,1,5,2,1)).take(6).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 6
                                                  //| 13
                                                  //| 19
 denominatorStream(Stream(0,1,5,2,2)).take(6).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 6
                                                  //| 13
                                                  //| 32
 
 
 
 val cf = confinuedFractionCoefficientsOfRootNplusKoverD(3, ki=1, di=1)
                                                  //> cf  : Stream[Int] = Stream(2, ?)
 val d = denominatorStream(cf).drop(1)//.take(100).foreach(println)
                                                  //> d  : Iterator[BigInt] = non-empty iterator
 val n = numeratorStream(cf).drop(1)              //> n  : Iterator[BigInt] = non-empty iterator
 
 val out = cf zip n.toStream zip d.toStream map {case ((cf,n), d) => (cf, n, d, n.toDouble / d.toDouble )}
                                                  //> out  : scala.collection.immutable.Stream[(Int, BigInt, BigInt, Double)] = S
                                                  //| tream((2,2,1,2.0), ?)
 // Reproduces the table at http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html#section9.3
 out.take(10).foreach(println)                    //> (2,2,1,2.0)
                                                  //| (1,3,1,3.0)
                                                  //| (2,8,3,2.6666666666666665)
                                                  //| (1,11,4,2.75)
                                                  //| (2,30,11,2.727272727272727)
                                                  //| (1,41,15,2.7333333333333334)
                                                  //| (2,112,41,2.731707317073171)
                                                  //| (1,153,56,2.732142857142857)
                                                  //| (2,418,153,2.7320261437908497)
                                                  //| (1,571,209,2.7320574162679425)
 

 confinuedFractionCoefficientsOfRootNplusKoverD(3, 0, 1).take(10).foreach(println)
                                                  //> 1
                                                  //| 1
                                                  //| 2
                                                  //| 1
                                                  //| 2
                                                  //| 1
                                                  //| 2
                                                  //| 1
                                                  //| 2
                                                  //| 1
 // Making sure that it runs quickly. The Stream implemnation as in numeratorStream won't be able to handle n = 100.
 denominatorStream(confinuedFractionCoefficientsOfRootNplusKoverD(3, 0, 1)).take(100).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 4
                                                  //| 11
                                                  //| 15
                                                  //| 41
                                                  //| 56
                                                  //| 153
                                                  //| 209
                                                  //| 571
                                                  //| 780
                                                  //| 2131
                                                  //| 2911
                                                  //| 7953
                                                  //| 10864
                                                  //| 29681
                                                  //| 40545
                                                  //| 110771
                                                  //| 151316
                                                  //| 413403
                                                  //| 564719
                                                  //| 1542841
                                                  //| 2107560
                                                  //| 5757961
                                                  //| 7865521
                                                  //| 21489003
                                                  //| 29354524
                                                  //| 80198051
                                                  //| 109552575
                                                  //| 299303201
                                                  //| 408855776
                                                  //| 1117014753
                                                  //| 1525870529
                                                  //| 4168755811
                                                  //| 5694626340
                                                  //| 15558008491
                                                  //| 21252634831
                                                  //| 58063278153
                                                  //| 79315912984
                                                  //| 216695104121
                                                  //| 296011017105
                                                  //| 808717138331
                                                  //| 1104728155436
                                                  //| 3018173449203
                                                  //| 4122901604639
                                                  //| 11263976658481
                                                  //| 15386878263120
                                                  //| 42037733184721
                                                  //| 57424611447841
                                                  //| 156886956080403
                                                  //| 214311567528244
                                                  //| 585510091136891
                                                  //| 799821658665135
                                                  //| 2185153408467161
                                                  //| 2984975067132296
                                                  //| 8155103542731753
                                                  //| 11140078609864049
                                                  //| 30435260762459851
                                                  //| 41575339372323900
                                                  //| 113585939507107651
                                                  //| 155161278879431551
                                                  //| 423908497265970753
                                                  //| 579069776145402304
                                                  //| 1582
                                                  //| Output exceeds cutoff limit.
 //denominatorStream(cfOfRootNplusKoverD(3, 0, 1)).take(100).foreach(println)
 
 // Consider x = [a0; a1, …] and y = [b0; b1, …]. If k is the smallest index for which ak is unequal to bk then
 // x < y if (−1)^k * (ak − bk) < 0 and y < x otherwise.
 def lessThanContinuedFractions( a: Stream[Int], b: Stream[Int]): Boolean = {
   a.zip(b).zipWithIndex.find{ case ((ak,bk),k) => ak != bk } match {
    case Some(((ak,bk),k)) => math.pow(-1,k) * (ak-bk) < 0
    
    // If there is no such k, but one expansion is shorter than the other, say x = [a0; a1, …, an] and
    // y = [b0; b1, …, bn, bn + 1, …] with ai = bi for 0 ≤ i ≤ n, then x < y if n is even and y < x if n is odd.
    // Keep in mind that size() is going to return n+1, since a0;a1,..an has n+1 elements.
    case None => {
     val s = a.zip(b).size
     if (a.drop(s).isEmpty && !b.drop(s).isEmpty)
      (s -1)%2 == 0
     else if ( b.drop(s).isEmpty && !a.drop(s).isEmpty)
      (s -1)%2 != 0
     else // The are the same length and totally equal
      false
    }
   }
 }                                                //> lessThanContinuedFractions: (a: Stream[Int], b: Stream[Int])Boolean
 
 lessThanContinuedFractions(Stream(2,5,1), Stream(2,2))
                                                  //> res0: Boolean = true
 lessThanContinuedFractions(Stream(2,2), Stream(2,5,1))
                                                  //> res1: Boolean = false

 lessThanContinuedFractions(Stream(2,2,5,1), Stream(2))
                                                  //> res2: Boolean = false
 lessThanContinuedFractions(Stream(2),Stream(2,2,5,1))
                                                  //> res3: Boolean = true

   
 //  We define m=BAT(x,N) as the denominator, m, of a fraction k/m which is the best approximation for x with denominator < N.
 // From https://en.wikipedia.org/wiki/Continued_fraction#Best_rational_approximations
 //  One can choose to define a best rational approximation to a real number x as a rational number
 // n/d, d > 0, that is closer to x than any approximation with a smaller or equal denominator.
 def BAT(x: Int, N: BigInt) = {
  val dStream = denominatorStream(confinuedFractionCoefficientsOfRootNplusKoverD(x, 0, 1)).toStream.span(_ < N)  //takeWhile(_ < N).last
  val Stream(d1,d2) = dStream._1.takeRight(2)
  val d3 = dStream._2.head
  val ccalc = (d3-d1)/d2  // Verifying we've captured the right coeffiencients, by using the identity d1+c*d2 = d3
  val c = confinuedFractionCoefficientsOfRootNplusKoverD(x, 0, 1)(dStream._1.size-1)
  
  // Want to find max m such that d1 + m*d2 < N.  We already know that d1+c*d2 = d3 >= N
  // So m = floor( (N-d1)/d2 ), and m < c
  val m = (N-d1)/d2
  
 // println(d1,d2,d3,c,dStream._1.size,ccalc, m)
  
 /*
  The simple continued fraction for x generates all of the best rational approximations for x according to three rules:
  Truncate the continued fraction, and possibly reduce its last term.
  The reduced term cannot have less than half its original value.
  If the final term is even, half its value is admissible only if the corresponding semiconvergent
  is better than the previous convergent. (See below.)
  The "half rule" mentioned above is that when ak is even, the halved term ak/2 is admissible if and only if
  |x − [a0 ; a1, …, ak − 1]| > |x − [a0 ; a1, …, ak − 1, ak/2]|[11]
  This is equivalent[11] to:[12]
  [ak; ak − 1, …, a1] > [ak; ak + 1, …].
  
  [semiconvergents] are called intermediate fractions by Khinchin [1, pp. 13–15].
  They are called “Nebennäherungsbrüche” and “eingeschaltete Brüche” by Perron [8, II.16.I, p. 47]
  
https://books.google.ca/books?id=8CGj9_ZlFKoC&pg=PA25&redir_esc=y#v=onepage&q&f=false
Allow ck/2 whenever d_k-2 / d_k-1 > ak / a_k-1 (where a is numerator, and d is denominator).
or (since floating-point division would lose accurary) whenever d_k-2*a_k-1 > d_k-1 *ak.
  */
  if (m > c/2 || (c%2 == 0 && m == c/2 &&
  // [ak; ak + 1, …] < [ak; ak − 1, …, a1]  // The right side, we're dropping the a0 term.
   lessThanContinuedFractions(
    confinuedFractionCoefficientsOfRootNplusKoverD(x, 0, 1).drop(dStream._1.size-1),
    confinuedFractionCoefficientsOfRootNplusKoverD(x, 0, 1).drop(1).take(dStream._1.size-1).reverse
  )))
   d1+m*d2  // the highest semiconvergent
  else
   d2 // the highest convergent.
 }                                                //> BAT: (x: Int, N: BigInt)scala.math.BigInt
 
 confinuedFractionCoefficientsOfRootNplusKoverD(212, 0, 1).take(10).foreach(println)
                                                  //> 14
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 6
                                                  //| 1
                                                  //| 1
  confinuedFractionCoefficientsOfRootNplusKoverD(212, 0, 1).drop(7).take(10).foreach(println)
                                                  //> 6
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 28
                                                  //| 1
                                                  //| 1
  confinuedFractionCoefficientsOfRootNplusKoverD(212, 0, 1).drop(1).take(7).reverse
                                                  //> res4: scala.collection.immutable.Stream[Int] = Stream(6, 1, 1, 1, 3, 1, 1)
  
  denominatorStream(confinuedFractionCoefficientsOfRootNplusKoverD(212, 0, 1)).take(10).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 2
                                                  //| 7
                                                  //| 9
                                                  //| 16
                                                  //| 25
                                                  //| 166
                                                  //| 191
 
  BAT(212,100)                                    //> res5: scala.math.BigInt = 25
  BAT(212,N1)                                     //> res6: scala.math.BigInt = 5929223246159194801
  BAT(53,N1)                                      //> res7: scala.math.BigInt = 5929223246159194801
  BAT(477,N1)                                     //> res8: scala.math.BigInt = 5929223246159194801
  BAT(212,N2)                                     //> res9: scala.math.BigInt = 17217982601
  BAT(53,N2)                                      //> res10: scala.math.BigInt = 17217982601
  BAT(477,N2)                                     //> res11: scala.math.BigInt = 17217982601
  BAT(53,10000)                                   //> res12: scala.math.BigInt = 9100
  BAT(212,10000)                                  //> res13: scala.math.BigInt = 4550
  BAT(477,10000)                                  //> res14: scala.math.BigInt = 9100

 //val b1To_n_Map = scala.collection.mutable.Map[BigInt,Int]()
 val b1To_n_MultiMap = new HashMap[BigInt, scala.collection.mutable.Set[Int]]() with MultiMap[BigInt, Int]
                                                  //> b1To_n_MultiMap  : scala.collection.mutable.HashMap[BigInt,scala.collectio
                                                  //| n.mutable.Set[Int]] with scala.collection.mutable.MultiMap[BigInt,Int] = M
                                                  //| ap()
 // Find two            integers n1 and n2 such that    BAT(sqrt(n1),N1)=BAT(sqrt(n2),N1) and
 //    BAT(sqrt(n1),N2)=BAT(sqrt(n2),N2) and both those numbers are prime numbers.
 
 // Find two non-square integers n1 and n2 such that b1=BAT(sqrt(n1),N1)=BAT(sqrt(n2),N1) and
 // b2=BAT(sqrt(n1),N2)=BAT(sqrt(n2),N2) and both   b1 and b2   are prime numbers.

 val solutions = Stream.from(2).filter{ n => /* Filter out square numbers*/
   math.sqrt(n) != math.sqrt(n).toInt
  }.flatMap{ n2=>
   val b1 = BAT(n2, N2)
   if (b1.isProbablePrime(100))
   {
    b1To_n_MultiMap.get(b1) match {
     case Some(n1Set) => {
     	// Now see if n1 and n2 work for N1.
			val b2 = BAT(n2, N1)
      if (b2.isProbablePrime(100) && n1Set.exists(n1 => b2 == BAT(n1, N1)))
      {
       // println("SOLUTION FOUND n", n1, "b1", b1, "b2 ", b2 , "n2 ", n2)
       n1Set.find(n1 => b2 == BAT(n1,N1)) match {
       case Some(n1) =>  Some((n1, n2, n2.toDouble/n1, b1, b2))
       case _ => None
       }
      }
      else
       None
      }
     case None =>
     {
      b1To_n_MultiMap.addBinding(b1, n2)
      None
     }
    }
   }
   else
    None
  }                                               //> solutions  : scala.collection.immutable.Stream[(Int, Int, Double, scala.ma
                                                  //| th.BigInt, scala.math.BigInt)] = Stream((53,212,4.0,17217982601,5929223246
                                                  //| 159194801), ?)
                                                  
 solutions.take(2).foreach(println)               //> (53,212,4.0,17217982601,5929223246159194801)
                                                  //| (53,477,9.0,17217982601,5929223246159194801)
/*
  (53,212,17217982601,5929223246159194801)
(53,477,17217982601,5929223246159194801)
3741,33669,16656342559,3797886248130533129)
12494,49976,17062022209,5436101419659921421)
15505,62020,12015469787,4978364727578578811)
21383,85532,7335053977,6036436113575426627)
22046,88184,9682948903,2585530461664755041)
23361,93444,17590003583,3621261582751083251)
23362,93448,5423913017,1131494199280660673)
26142,104568,12679718803,1686451596050501171)
27917,111668,11251269037,4943079314587400209)
12494,112446,17062022209,5436101419659921421)
31158,124632,11688046891,6228332288473791649)
32177,128708,10476481553,1602286751357325289)
32554,130216,13246827149,3378564568695461609)
37851,151404,14555424553,4507669400707720847)
38876,155504,3270865349,5218799837842808251)
40123,160492,8983199377,4754869691931640189)
41394,165576,9445796617,5234795125381063031)
44724,178896,1721828267,61106276
                                                  */

 solutions.find{ case (n1,n2,_,_,_) => BAT(n1,10000) == BAT(n2,10000) }
                                                  //> res15: Option[(Int, Int, Double, scala.math.BigInt, scala.math.BigInt)] = 
                                                  //| Some((53,477,9.0,17217982601,5929223246159194801))
  
  // for the smallest solution that also satisfy BAT(sqrt(n1),10000)=BAT(sqrt(n2),10000),
  // what is the connection between BAT(sqrt(n1),10000) and IBM
 val b1To_n_MultiMapBonus = new HashMap[BigInt, scala.collection.mutable.Set[Int]]() with MultiMap[BigInt, Int]
                                                  //> b1To_n_MultiMapBonus  : scala.collection.mutable.HashMap[BigInt,scala.coll
                                                  //| ection.mutable.Set[Int]] with scala.collection.mutable.MultiMap[BigInt,Int
                                                  //| ] = Map()
 
 val solutionsBonus = Stream.from(2).filter{ n => /* Filter out square numbers*/
   math.sqrt(n) != math.sqrt(n).toInt
  }.flatMap{ n2=>
   val b1 = BAT(n2, 10000)
   b1To_n_MultiMapBonus.get(b1) match {
    case Some(n1Set) => {
       // println("SOLUTION FOUND n", n1, "b1", b1, "b2 ", b2 , "n2 ", n2)
       Some((n1Set, n2, n2.toDouble/n1Set.head, b1))
       }
     case None =>
     {
      b1To_n_MultiMapBonus.addBinding(b1, n2)
      None
     }
    }
   }                                              //> solutionsBonus  : scala.collection.immutable.Stream[(scala.collection.muta
                                                  //| ble.Set[Int], Int, Double, scala.math.BigInt)] = Stream((Set(5),20,4.0,547
                                                  //| 3), ?)
   
     
  solutionsBonus.take(2000).toList.sortBy(_._2).take(20).foreach(println)
                                                  //> (Set(5),20,4.0,5473)
                                                  //| (Set(11),44,4.0,7561)
                                                  //| (Set(13),52,4.0,6485)
                                                  //| (Set(17),68,4.0,4289)
                                                  //| (Set(24),96,4.0,9701)
                                                  //| (Set(29),116,4.0,9801)
                                                  //| (Set(13),117,9.0,6485)
                                                  //| (Set(32),128,4.0,3465)
                                                  //| (Set(38),152,4.0,5401)
                                                  //| (Set(40),160,4.0,4329)
                                                  //| (Set(43),172,4.0,7199)
                                                  //| (Set(24),186,7.75,9701)
                                                  //| (Set(47),188,4.0,9311)
                                                  //| (Set(51),204,4.0,9899)
                                                  //| (Set(50),205,4.1,2772)
                                                  //| (Set(24),216,9.0,9701)
                                                  //| (Set(93),242,2.6021505376344085,1260)
                                                  //| (Set(61),244,4.0,3805)
                                                  //| (Set(62),248,4.0,9953)
                                                  //| (Set(66),264,4.0,9489)
  solutionsBonus.filter(_._1.head < 5).take(20).foreach(println)
                                                  //> (Set(3),3118,1039.3333333333333,7953)
                                                  //| (Set(3),5366,1788.6666666666667,7953)
                                                  //| (Set(3),7647,2549.0,7953)
                                                  //| (Set(2),14913,7456.5,5741)
                                                  //| (Set(2),22123,11061.5,5741)
                                                  //| (Set(2),28171,14085.5,5741)
                                                  //| (Set(3),30588,10196.0,7953)
                                                  //| (Set(3),42878,14292.666666666666,7953)
                                                  //| (Set(2),44313,22156.5,5741)
                                                  //| (Set(2),53755,26877.5,5741)
                                                  //| (Set(2),55495,27747.5,5741)
                                                  //| (Set(3),69435,23145.0,7953)
                                                  //| (Set(2),69556,34778.0,5741)
                                                  //| (Set(2),73921,36960.5,5741)
                                                  //| (Set(3),77979,25993.0,7953)
                                                  //| (Set(3),79585,26528.333333333332,7953)
                                                  //| (Set(2),82214,41107.0,5741)
                                                  //| (Set(2),87073,43536.5,5741)
                                                  //| (Set(3),96735,32245.0,7953)
                                                  //| (Set(2),98547,49273.5,5741)
  Stream.from(2).filter{ n => /* Filter out square numbers*/
   math.sqrt(n) != math.sqrt(n).toInt
  }.map(n=>(n,BAT(n,10000))).take(20).foreach(println)
                                                  //> (2,5741)
                                                  //| (3,7953)
                                                  //| (5,5473)
                                                  //| (6,8721)
                                                  //| (7,7873)
                                                  //| (8,6930)
                                                  //| (10,8658)
                                                  //| (11,7561)
                                                  //| (12,5432)
                                                  //| (13,6485)
                                                  //| (14,3596)
                                                  //| (15,3905)
                                                  //| (17,4289)
                                                  //| (18,4620)
                                                  //| (19,8501)
                                                  //| (20,5473)
                                                  //| (21,8689)
                                                  //| (22,5123)
                                                  //| (23,9164)
                                                  //| (24,9701)

// Looking for a 5473???
// IBM (NYSE: IBM) inventors received a record 9,100 patents in 2018,
 Stream.from(2).filter{ n => /* Filter out square numbers*/
   math.sqrt(n) != math.sqrt(n).toInt
  }.map(n=>(n,BAT(n,10000))).filter(_._2 == 9100).take(10).foreach(println)
                                                  //> (53,9100)
                                                  //| (477,9100)
                                                  //| (6413,9100)
                                                  //| (19133,9100)
                                                  //| (26094,9100)
                                                  //| (46050,9100)
                                                  //| (77601,9100)
                                                  //| (102460,9100)
                                                  //| (186361,9100)
                                                  //| (194541,9100)
  
// (Set(11),44,7561)
// So we're looking for 7561???
// The IBM 7561 belongs to a side-development of IBM's Personal Computer Division that builds PCs for industrial use.
// http://ohlandl.ipv7.net/7561-62/ibm7561.html
// ARMONK, N.Y.	- 09 Jan 2018: IBM (NYSE: IBM) inventors received a record 9,043 patents in 2017
//
                                                  
  // Set(5040),5042,142)
// (Set(5183),5185,144)
  
  confinuedFractionCoefficientsOfRootNplusKoverD(212, 0, 1).take(50).foreach(println)
                                                  //> 14
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 6
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 28
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 6
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 28
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 6
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 28
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 6
  denominatorStream(confinuedFractionCoefficientsOfRootNplusKoverD(212, 0, 1)).take(50).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 2
                                                  //| 7
                                                  //| 9
                                                  //| 16
                                                  //| 25
                                                  //| 166
                                                  //| 191
                                                  //| 357
                                                  //| 548
                                                  //| 2001
                                                  //| 2549
                                                  //| 4550
                                                  //| 129949
                                                  //| 134499
                                                  //| 264448
                                                  //| 927843
                                                  //| 1192291
                                                  //| 2120134
                                                  //| 3312425
                                                  //| 21994684
                                                  //| 25307109
                                                  //| 47301793
                                                  //| 72608902
                                                  //| 265128499
                                                  //| 337737401
                                                  //| 602865900
                                                  //| 17217982601
                                                  //| 17820848501
                                                  //| 35038831102
                                                  //| 122937341807
                                                  //| 157976172909
                                                  //| 280913514716
                                                  //| 438889687625
                                                  //| 2914251640466
                                                  //| 3353141328091
                                                  //| 6267392968557
                                                  //| 9620534296648
                                                  //| 35128995858501
                                                  //| 44749530155149
                                                  //| 79878526013650
                                                  //| 2281348258537349
                                                  //| 2361226784550999
                                                  //| 4642575043088348
                                                  //| 16288951913816043
                                                  //| 20931526956904391
                                                  //| 37220478870720434
                                                  //| 58152005827624825
  confinuedFractionCoefficientsOfRootNplusKoverD(53, 0, 1).take(50).foreach(println)
                                                  //> 7
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
                                                  //| 14
                                                  //| 3
                                                  //| 1
                                                  //| 1
                                                  //| 3
  denominatorStream(confinuedFractionCoefficientsOfRootNplusKoverD(53, 0, 1)).take(50).foreach(println)
                                                  //> 0
                                                  //| 1
                                                  //| 3
                                                  //| 4
                                                  //| 7
                                                  //| 25
                                                  //| 357
                                                  //| 1096
                                                  //| 1453
                                                  //| 2549
                                                  //| 9100
                                                  //| 129949
                                                  //| 398947
                                                  //| 528896
                                                  //| 927843
                                                  //| 3312425
                                                  //| 47301793
                                                  //| 145217804
                                                  //| 192519597
                                                  //| 337737401
                                                  //| 1205731800
                                                  //| 17217982601
                                                  //| 52859679603
                                                  //| 70077662204
                                                  //| 122937341807
                                                  //| 438889687625
                                                  //| 6267392968557
                                                  //| 19241068593296
                                                  //| 25508461561853
                                                  //| 44749530155149
                                                  //| 159757052027300
                                                  //| 2281348258537349
                                                  //| 7003801827639347
                                                  //| 9285150086176696
                                                  //| 16288951913816043
                                                  //| 58152005827624825
                                                  //| 830417033500563593
                                                  //| 2549403106329315604
                                                  //| 3379820139829879197
                                                  //| 5929223246159194801
                                                  //| 21167489878307463600
                                                  //| 302274081542463685201
                                                  //| 927989734505698519203
                                                  //| 1230263816048162204404
                                                  //| 2158253550553860723607
                                                  //| 7705024467709744375225
                                                  //| 110028596098490281976757
                                                  //| 337790812763180590305496
                                                  //| 447819408861670872282253
                                                  //| 785610221624851462587749
    val s1 = 10000*10000 +1                       //> s1  : Int = 100000001

val s2 = 10001*10001 +1                           //> s2  : Int = 100020002
BAT(s1,10000)                                     //> res16: scala.math.BigInt = 1
BAT(s2,10000)                                     //> res17: scala.math.BigInt = 1
// No need to brute force this since we've found a bunch of BAT values = 1.
//  solutionsBonus.take(20000).toList.sortBy(_._4).take(200).foreach(println)
 
 def main(args: Array[String]) = {
  //solutions.foreach(println)
 

 }                                                //> main: (args: Array[String])Unit
}