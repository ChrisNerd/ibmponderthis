import scala.util.Random

object ponder {
  /*
3-Smooth Representations of Integers
Richard Blecksmith, Michael McCallum and J. L. Selfridge
https://www.jstor.org/stable/2589404
In February 1994 issue of Mathematics Magazine, Paul Erdos posed the following problem: Prove that any positive integer can be written as the sum of terms of the form 2^a 3^b, where
no summand divides another. This innocuous question in the "Quickies" section has led to several interesting and "not-so-quick"results and conjectures.
....


https://blog.plover.com/math/pow23tab.html
I think this is haskell

    maxPowerOf3 1 = 1
    maxPowerOf3 2 = 1
    maxPowerOf3 n = 3 * maxPowerOf3 (n `div` 3)

    rep :: Integer -> [Integer]
    rep 0 = []
    rep n = if even n then map (* 2) (rep (n `div` 2))
        else (rep (n - mp3)) ++ [mp3] where mp3 = maxPowerOf3 n

main = print (rep ((10)^(199)))



*/

  // The log3(n).floor... returns the largest{1,3,9,27...} that's <= n.
    def maxPowerOf3(n: BigInt): BigInt = n match {
    case n if n < 3 => 1
    case _          => 3 * maxPowerOf3(n / 3)
  }                                               //> maxPowerOf3: (n: BigInt)BigInt


  // return 3^i <= n (the index i, instead of the evaluation 1,3,9,etc)
  // Basically take the log3 of everything above
    def maxPowerOf3Index(n: BigInt): Int = n match {
    case n if n < 3 => 0
    case _          => 1 + maxPowerOf3Index(n / 3)
  }                                               //> maxPowerOf3Index: (n: BigInt)Int
  
  import scala.collection.Searching._
  val numDigitsOfN = 300                          //> numDigitsOfN  : Int = 300
  val allOnes = (0 until numDigitsOfN).map(x => BigInt(10).pow(x)).sum
                                                  //> allOnes  : scala.math.BigInt = 11111111111111111111111111111111111111111111
                                                  //| 111111111111111111111111111111111111111111111111111111111111111111111111111
                                                  //| 111111111111111111111111111111111111111111111111111111111111111111111111111
                                                  //| 111111111111111111111111111111111111111111111111111111111111111111111111111
                                                  //| 1111111111111111111111111111111
  val powersOf3 = Stream.from(0).map(x => BigInt(3).pow(x)).takeWhile(x => x < allOnes).toIndexedSeq
                                                  //> powersOf3  : scala.collection.immutable.IndexedSeq[scala.math.BigInt] = Vec
                                                  //| tor(1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 15
                                                  //| 94323, 4782969, 14348907, 43046721, 129140163, 387420489, 1162261467, 34867
                                                  //| 84401, 10460353203, 31381059609, 94143178827, 282429536481, 847288609443, 2
                                                  //| 541865828329, 7625597484987, 22876792454961, 68630377364883, 20589113209464
                                                  //| 9, 617673396283947, 1853020188851841, 5559060566555523, 16677181699666569, 
                                                  //| 50031545098999707, 150094635296999121, 450283905890997363, 1350851717672992
                                                  //| 089, 4052555153018976267, 12157665459056928801, 36472996377170786403, 10941
                                                  //| 8989131512359209, 328256967394537077627, 984770902183611232881, 29543127065
                                                  //| 50833698643, 8862938119652501095929, 26588814358957503287787, 7976644307687
                                                  //| 2509863361, 239299329230617529590083, 717897987691852588770249, 21536939630
                                                  //| 75557766310747, 6461081889226673298932241, 19383245667680019896796723, 5814
                                                  //| 9737003040059690390169,
                                                  //| Output exceeds cutoff limit.
  val powersOf2 = Stream.from(0).map(x => BigInt(2).pow(x)).takeWhile(x => x < allOnes).toIndexedSeq
                                                  //> powersOf2  : scala.collection.immutable.IndexedSeq[scala.math.BigInt] = Vec
                                                  //| tor(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 3
                                                  //| 2768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16
                                                  //| 777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 21
                                                  //| 47483648, 4294967296, 8589934592, 17179869184, 34359738368, 68719476736, 13
                                                  //| 7438953472, 274877906944, 549755813888, 1099511627776, 2199023255552, 43980
                                                  //| 46511104, 8796093022208, 17592186044416, 35184372088832, 70368744177664, 14
                                                  //| 0737488355328, 281474976710656, 562949953421312, 1125899906842624, 22517998
                                                  //| 13685248, 4503599627370496, 9007199254740992, 18014398509481984, 3602879701
                                                  //| 8963968, 72057594037927936, 144115188075855872, 288230376151711744, 5764607
                                                  //| 52303423488, 1152921504606846976, 2305843009213693952, 4611686018427387904,
                                                  //|  9223372036854775808, 18446744073709551616, 36893488147419103232, 737869762
                                                  //| 94838206464, 1475739525
                                                  //| Output exceeds cutoff limit.
  // Can do this faster with a list of powers of 3. It will be sorted so it can be searched in log(n) time.
  def maxPowerOf3IndexUsingTable(n: BigInt): Int = powersOf3.search(n + 1).insertionPoint - 1
                                                  //> maxPowerOf3IndexUsingTable: (n: BigInt)Int
  def maxPowerOf2IndexUsingTable(n: BigInt): Int = powersOf2.search(n + 1).insertionPoint - 1
                                                  //> maxPowerOf2IndexUsingTable: (n: BigInt)Int
  
  maxPowerOf3IndexUsingTable(26)                  //> res0: Int = 2
  maxPowerOf3IndexUsingTable(27)                  //> res1: Int = 3
  maxPowerOf3IndexUsingTable(28)                  //> res2: Int = 3
  maxPowerOf2IndexUsingTable(1023)                //> res3: Int = 9
  maxPowerOf2IndexUsingTable(1024)                //> res4: Int = 10
  maxPowerOf2IndexUsingTable(1025)                //> res5: Int = 10

  def validateSum(r: List[(Int,Int)], rando: BigInt) = r.map(x => BigInt(2).pow(x._1) * BigInt(3).pow(x._2)).sum == rando
                                                  //> validateSum: (r: List[(Int, Int)], rando: BigInt)Boolean
  def validateDivisibilityRule(r: List[(Int,Int)]) = r.sortBy(_._1).sliding(2).forall(x => x(1)._2 < x(0)._2)
                                                  //> validateDivisibilityRule: (r: List[(Int, Int)])Boolean
  

  
  

  // So if n is even, we can just /2, and recurse. The result of represent(n/2) we can multiply every entry by 2. This will not affect their relative divisibility,
  // as it can be thought of as shifting the whole set one unit to the right.
  // if n is odd, we can just subtract off the largest power of 3, and take it on the end.
  // the result of n(odd) - largest power of 3 will be even!
  // This is very similar to 3n+1. Collatz conjecture

  // How can we prove that all the entries will not divide each other?
  // represent(n/2).map(_ * 2) will be fine if represent(n/2) is. If represent(n/2) is not good, then shifting it will not help! Eventually n/2 will be odd since it must hit 1 at least.
  // if n is odd, then there are two cases
  // n is a perfect power of 3, or it is not.
  // if n is a perfect power of 3, we select that one entry and are done.
  // otherwise we select the highest power of 3 less than n, and recurse on the even number {n - power of 3}
  // The highest power of 3 must preclude any lower power of 3, this is true because the recurse on the even number will (at least) double all the entries, so there's no possibily of any odd entries
  // The subtraction of the highest power of 3 will return a fairly small number too. This number will definitely be smaller than the highest power of 3.
  // n - 3^ log3(n).floor < 3^log3(n).floor . This is because n < 2*3 ^ log3(n)

  // hmmm, might need to prove that after a good night's sleep

  // let d = log3(n) // Real number
  // i = floor(d) // Int  highest power of 3
  // f = d - i // is [0, 1)
  // what is n - 3^i ?
  // Is it < 3^i ?
  // Let's try to prove n - 3^i < 3^i.
  // n < 2 * 3^i
  // let's take log3 of both sides
  // log3(n) < log3(2) + i
  // d = log3(n)
  // d = i + f < log3(2) + i
  // AHH! It's not necessiraly true
  // n=8.
  // i = 1
  // 3^i = 3.
  // 8 - 3 =5 > 3
  /*
  def represent(n: BigInt): List[BigInt] = n match {
    case n if n == 0 => List()
    case n if (n % 2 == 0) => represent(n / 2).map(_ * 2)
    case _ => maxPowerOf3(n) :: represent(n - maxPowerOf3(n))
  }
*/
  
  def representPairs(n: BigInt): List[(Int, Int)] = n match {
    case n if n == 0 => List()
    case n if (n % 2 == 0) => representPairs(n / 2).map{ case (a, b) => (a + 1, b) }
    case _ =>    {
      val m = maxPowerOf3IndexUsingTable(n)
      (0, m) :: representPairs(n - powersOf3(m))
      }
  }                                               //> representPairs: (n: BigInt)List[(Int, Int)]
  
  
  
 /*
  println("represent(BigInt(100)) " + represent(BigInt(100)))
  println("representPairs(BigInt(100)) " + representPairs(BigInt(100)))
*/
  def representmod6Iterator(n: BigInt): List[(Int, Int)] = n match {
    case n if n == 0       => List()
  //  case n if (n % 6 == 0) => representmod6Iterator(n / 6).map{ x => x.map{ case (a, b) => (a + 1, b + 1) } }
    case n if (n % 6 == 0) => representmod6Iterator(n / 6).map{ case (a, b) => (a + 1, b + 1) }
    case n if (n % 4 == 0) => representmod6Iterator(n / 4).map{ case (a, b) => (a + 2, b) }
    case n if (n % 2 == 0) => representmod6Iterator(n / 2).map{ case (a, b) => (a + 1, b) }
    case n if (n % 3 == 0) => representmod6Iterator(n / 3).map{ case (a, b) => (a, b + 1) }
    case n if (n % 6 == 1 || n % 6 == 5) => {
      val m = maxPowerOf3IndexUsingTable(n)
      // Hmmm how about
      /*val hmmm = for {
       //mm <- 0 to m
       mm <- scala.math.max(0,(m-1)) to m
       rest <- representmod6Iterator(n - powersOf3(mm))
      } yield (0, mm) :: rest
      */
      
      val minm = (0 to m).maxBy(x => powersOf2.takeWhile(p => (n - powersOf3(x)) % p == 0).size )
      
      //  ok. this gives an assert error, the divisibility test fails!
      // looks like we do need the max power of 3, not just any power of 3.
      ( 0, minm) :: representmod6Iterator(n - powersOf3(minm))
    }
  }                                               //> representmod6Iterator: (n: BigInt)List[(Int, Int)]


 val n = BigInt("121415161")                      //> n  : scala.math.BigInt = 121415161
  representmod6Iterator(n)                        //> res6: List[(Int, Int)] = List((0,14), (7,0), (9,0), (11,2), (13,3), (20,1),
                                                  //|  (22,3))

      val m = maxPowerOf3IndexUsingTable(n)       //> m  : Int = 16
      
      val minm3 = (0 to m).map(x => (n - powersOf3(x)))
                                                  //> minm3  : scala.collection.immutable.IndexedSeq[scala.math.BigInt] = Vector(
                                                  //| 121415160, 121415158, 121415152, 121415134, 121415080, 121414918, 121414432
                                                  //| , 121412974, 121408600, 121395478, 121356112, 121238014, 120883720, 1198208
                                                  //| 38, 116632192, 107066254, 78368440)
      
      val minm2 = (0 to m).map(x => maxPowerOf2IndexUsingTable(n - powersOf3(x)))
                                                  //> minm2  : scala.collection.immutable.IndexedSeq[Int] = Vector(26, 26, 26, 26
                                                  //| , 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
      val minm4 = (0 to m).map{x =>
             powersOf2.takeWhile(p => (n - powersOf3(x)) % p == 0).size
      }                                           //> minm4  : scala.collection.immutable.IndexedSeq[Int] = Vector(4, 2, 5, 2, 4,
                                                  //|  2, 6, 2, 4, 2, 5, 2, 4, 2, 8, 2, 4)
      val minm = (0 to m).maxBy{x =>
      // which of these numbers is the most divisible by 2. Like we'd prefer a number that = 0 mod 1024 over one that is only 0 mod 256.
       powersOf2.takeWhile(p => (n - powersOf3(x)) % p == 0).size
      }                                           //> minm  : Int = 14
      
//      hmmm.minBy(x => maxPowerOf2( x.tail ))
      ( 0, minm) :: representmod6Iterator(n - powersOf3(minm))
                                                  //> res7: List[(Int, Int)] = List((0,14), (7,0), (9,0), (11,2), (13,3), (20,1),
                                                  //|  (22,3))

  def representmod6(n: BigInt): List[(Int, Int)] = n match {
    case n if n == 0       => List()
    case n if (n % 6 == 0) => representmod6(n / 6).map { case (a, b) => (a + 1, b + 1) }
    case n if (n % 4 == 0) => representmod6(n / 4).map { case (a, b) => (a + 2, b) }
    case n if (n % 2 == 0) => representmod6(n / 2).map { case (a, b) => (a + 1, b) }
    case n if (n % 3 == 0) => representmod6(n / 3).map { case (a, b) => (a, b + 1) }
    case n if (n % 6 == 1 || n % 6 == 5) => {
      val m = maxPowerOf3IndexUsingTable(n)
      (0, m) :: representmod6(n - powersOf3(m))
    }
  }                                               //> representmod6: (n: BigInt)List[(Int, Int)]

  println("representmod6(100 ) " + representmod6(BigInt(100)))
                                                  //> representmod6(100 ) List((2,2), (6,0))

  val sol = "11101010000111100010111100001100100100110000000111100000111100001111010100111101100111111000100100001110110000111000000110010000011110000111011001101111000111001000111100100110111000111100110111001100"
                                                  //> sol  : String = 11101010000111100010111100001100100100110000000111100000111
                                                  //| 100001111010100111101100111111000100100001110110000111000000110010000011110
                                                  //| 000111011001101111000111001000111100100110111000111100110111001100
  println("sol " + sol)                           //> sol 11101010000111100010111100001100100100110000000111100000111100001111010
                                                  //| 100111101100111111000100100001110110000111000000110010000011110000111011001
                                                  //| 101111000111001000111100100110111000111100110111001100
  println("sol.size " +sol.size)                  //> sol.size 200
  val solBigInt = BigInt(sol)                     //> solBigInt  : scala.math.BigInt = 111010100001111000101111000011001001001100
                                                  //| 000001111000001111000011110101001111011001111110001001000011101100001110000
                                                  //| 001100100000111100001110110011011110001110010001111001001101110001111001101
                                                  //| 11001100
  println("solBigInt "+ solBigInt)                //> solBigInt 11101010000111100010111100001100100100110000000111100000111100001
                                                  //| 111010100111101100111111000100100001110110000111000000110010000011110000111
                                                  //| 011001101111000111001000111100100110111000111100110111001100
  solBigInt.toString().size                       //> res8: Int = 200
  println("solBigInt.toString().size "+ solBigInt.toString().size)
                                                  //> solBigInt.toString().size 200
  println("representPairs(solBigInt) " + representPairs(solBigInt))
                                                  //> representPairs(solBigInt) List((2,415), (5,413), (8,411), (10,407), (13,404
                                                  //| ), (14,403), (24,396), (25,393), (26,392), (30,389), (33,381), (34,380), (3
                                                  //| 5,375), (38,373), (40,371), (41,369), (43,368), (45,365), (47,363), (49,361
                                                  //| ), (50,359), (53,356), (56,353), (60,350), (62,348), (63,347), (74,339), (7
                                                  //| 5,338), (76,337), (77,336), (78,335), (79,334), (81,333), (82,330), (85,325
                                                  //| ), (86,321), (87,320), (89,317), (91,313), (93,310), (97,305), (99,304), (1
                                                  //| 03,300), (105,298), (106,297), (107,296), (109,294), (113,292), (118,285), 
                                                  //| (119,284), (124,278), (127,276), (128,275), (130,272), (132,269), (133,268)
                                                  //| , (135,263), (136,261), (140,258), (141,257), (142,256), (149,251), (151,24
                                                  //| 3), (152,242), (154,241), (159,237), (163,232), (164,231), (165,230), (170,
                                                  //| 226), (179,219), (183,216), (187,214), (189,210), (191,208), (192,207), (19
                                                  //| 7,203), (199,202), (201,199), (207,193), (208,191), (210,189), (212,187), (
                                                  //| 214,185), (215,184), (2
                                                  //| Output exceeds cutoff limit.
  println("representPairs(solBigInt).size " + representPairs(solBigInt).size)
                                                  //> representPairs(solBigInt).size 150
  println("representmod6Iterator(solBigInt) " + representmod6Iterator(solBigInt))
                                                  //> representmod6Iterator(solBigInt) List((2,97), (15,4), (17,90), (28,160), (3
                                                  //| 9,4), (41,208), (54,4), (56,146), (66,4), (68,356), (80,179), (90,159), (10
                                                  //| 0,3), (102,4), (104,219), (115,5), (117,31), (128,325), (139,210), (149,6),
                                                  //|  (151,152), (162,256), (172,151), (182,65), (192,100), (202,268), (215,5), 
                                                  //| (217,215), (228,37), (238,9), (240,210), (250,90), (260,149), (270,9), (272
                                                  //| ,9), (274,69), (285,9), (287,110), (298,9), (300,9), (302,97), (312,8), (31
                                                  //| 4,9), (316,32), (327,99), (337,116), (346,10), (348,131), (357,10), (359,10
                                                  //| ), (361,67), (370,10), (372,10), (374,9), (376,38), (386,10), (388,81), (39
                                                  //| 8,12), (400,126), (413,12), (415,116), (425,67), (434,12), (436,104), (445,
                                                  //| 84), (455,13), (457,13), (459,13), (461,13), (463,12), (465,12), (467,96), 
                                                  //| (483,42), (492,15), (494,68), (502,98), (512,78), (521,15), (523,50), (531,
                                                  //| 14), (533,16), (535,16), (537,16), (539,73), (547,16), (555,15), (557,16), 
                                                  //| (559,16), (561,16), (56
                                                  //| Output exceeds cutoff limit.
  println("representmod6Iterator(solBigInt).size " + representmod6Iterator(solBigInt).size)
                                                  //> representmod6Iterator(solBigInt).size 107
  println("representmod6(solBigInt) " + representmod6(solBigInt))
                                                  //> representmod6(solBigInt) List((2,415), (5,413), (8,411), (10,407), (13,404)
                                                  //| , (14,403), (24,396), (25,393), (26,392), (30,389), (33,381), (34,380), (35
                                                  //| ,375), (38,373), (40,371), (41,369), (43,368), (45,365), (47,363), (49,361)
                                                  //| , (50,359), (53,356), (56,353), (60,350), (62,348), (63,347), (74,339), (75
                                                  //| ,338), (76,337), (77,336), (78,335), (79,334), (81,333), (82,330), (85,325)
                                                  //| , (86,321), (87,320), (89,317), (91,313), (93,310), (97,305), (99,304), (10
                                                  //| 3,300), (105,298), (106,297), (107,296), (109,294), (113,292), (118,285), (
                                                  //| 119,284), (124,278), (127,276), (128,275), (130,272), (132,269), (133,268),
                                                  //|  (135,263), (136,261), (140,258), (141,257), (142,256), (149,251), (151,243
                                                  //| ), (152,242), (154,241), (159,237), (163,232), (164,231), (165,230), (170,2
                                                  //| 26), (179,219), (183,216), (187,214), (189,210), (191,208), (192,207), (197
                                                  //| ,203), (199,202), (201,199), (207,193), (208,191), (210,189), (212,187), (2
                                                  //| 14,185), (215,184), (21
                                                  //| Output exceeds cutoff limit.
  println("representmod6(solBigInt).size " + representmod6(solBigInt).size)
                                                  //> representmod6(solBigInt).size 150
  println("validateSum " + validateSum(representmod6(solBigInt), solBigInt))
                                                  //> validateSum true
  println("validateDivisibilityRule(representPairs(solBigInt)" + validateDivisibilityRule(representmod6(solBigInt)))
                                                  //> validateDivisibilityRule(representPairs(solBigInt)true

  var minSoFar = 20000                            //> minSoFar  : Int = 20000
  var runningAverage = 0                          //> runningAverage  : Int = 0
  var runningSum = 0                              //> runningSum  : Int = 0
  def average(ns: IndexedSeq[Int]): Double = if (runningSum == 0) 0 else runningSum / ns.size
                                                  //> average: (ns: IndexedSeq[Int])Double
  
  def standardDiviation(ns: IndexedSeq[Int], s: Int): Double = scala.math.sqrt(ns.map( x => scala.math.pow(x - average(ns),2)).sum / s )
                                                  //> standardDiviation: (ns: IndexedSeq[Int], s: Int)Double
  
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
}                                                 //> time: [R](block: => R)R
  

  val threshold = (1 until 10).map( _ / 10.0)     //> threshold  : scala.collection.immutable.IndexedSeq[Double] = Vector(0.1, 0.
                                                  //| 2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  val f = time { Stream.from(0)/*.take(1000)*//*.par*/.map { x =>
    val rando = BigInt(10).pow(numDigitsOfN - 1) + (0 until (numDigitsOfN - 1)).map(x =>
     if (Random.nextDouble() < threshold(x % threshold.size))
       BigInt(10).pow(x)
      else
       BigInt(0)
     ).sum
    val r = representmod6(rando)
    (r, rando, x)
    }.find{ case (r, rando, x) =>
    val smallestRep = r.size
    //println(r.size)
    if (x % 10000 == 0) {
      // 2^a 3^b.
      // Sort by the a's, and make sure than every pair goes down by the b's.
      /*
      
       mean= 183.3333, standard deviation = 7.6666, what's the probability of x<150
      8114949413627613/1180591620717411303424≈6.87363×10^-6
      Which means we can expect it to occor 1/6.87363×10^-6 = 145,483 which isn't too bad!
      ... if it is normally distributed
      
      I mean I did get to 151 once!
      */
      
      
      
      assert(validateDivisibilityRule(r), "divise " + r)
      
      assert(validateSum(r, rando), "sum")
      println("represnent r " + r)
      println("represnent rando " + rando)
      println("represent size " + smallestRep)
      println("minSoFar " + minSoFar)
      println("trials so far " + x)
    }

    if (minSoFar > smallestRep) {
      minSoFar = smallestRep
      println("New record represnent rando " + r)
      println("New record represnent rando " + rando)
      println("represent size " + smallestRep)
    }
    r.size < 215
  }
  }                                               //> represnent r List((3,624), (4,623), (5,622), (6,620), (8,619), (9,616), (1
                                                  //| 0,615), (11,614), (12,612), (14,611), (15,606), (16,604), (18,603), (20,60
                                                  //| 0), (24,597), (26,596), (28,593), (29,592), (30,591), (31,588), (33,586), 
                                                  //| (34,584), (39,581), (42,578), (49,573), (53,570), (54,569), (56,567), (57,
                                                  //| 566), (58,565), (59,564), (62,562), (63,561), (64,559), (68,554), (71,551)
                                                  //| , (75,548), (76,547), (77,546), (83,541), (84,540), (85,539), (88,537), (9
                                                  //| 1,534), (93,532), (95,529), (100,525), (103,523), (105,521), (107,520), (1
                                                  //| 11,515), (114,513), (116,511), (119,509), (120,507), (121,504), (127,499),
                                                  //|  (135,492), (136,489), (137,486), (142,483), (143,481), (144,480), (146,47
                                                  //| 9), (147,477), (151,474), (152,472), (156,469), (159,467), (161,464), (162
                                                  //| ,463), (163,461), (164,459), (165,458), (166,454), (169,451), (172,449), (
                                                  //| 179,442), (181,441), (183,437), (185,435), (186,432), (188,429), (189,427)
                                                  //| , (190,426), (191,423)
                                                  //| Output exceeds cutoff limit.

  /*
  println("rando " + rando)
  println("rep " + representPairs(rando).map(x => BigInt(2).pow(x._1) * BigInt(3).pow(x._2)).sum)
  println("rando.size " + rando.toString().size)

  println("Confirming " + (rando == representPairs(rando).map(x => BigInt(2).pow(x._1) * BigInt(3).pow(x._2)).sum))

  // 1/sqrt(6)*exp(sqrt(2*log(2)*log(3)*(138399+1)))
  def etox(x: BigDecimal, p: Int) =
    Stream.from(1).takeWhile(_ <= p)
      .foldLeft((BigDecimal(1), BigDecimal(1)) -> BigDecimal(1)) {
        case (((fac, pow), res), n) =>
          val f = fac * n
          val p = pow * x
          ((f, p), res + p / f)
      }._2

  //  val asymp = Stream.from(138399).map( n => 1.0/scala.math.sqrt(6.0) * etox( scala.math.sqrt(2.0 * scala.math.log(2)* scala.math.log(3)* n), 30000))

  /*
  f(a,b)=2^a*3^b


  b
  |\
  |  \
  |    \
  |      \
  |        \
  |__________\_____ ->a

  contour of when f=10^200

  I think the idea is to "construct" the target digit n, where n=something like
  110011101.... 200 digits long.
  by choosing points that contribute to the most significant digit first, then working
  our way down to the least significant digits.

  start with the points here
  b
  |\
  | x\
  |    \
  |      \
  |        \
  |_________x_\_____ ->a

  Add them to get something like
  10061878872.... 200 digits long.
  or even
  99992528726998 ... 199 digits long.
  but not
  1111628728979.... since there's no way to add anything else and stay with just 1s and 0s.

  Eventually, add points such that we get this kind of shape (cycloid???)

  b
  |\
  | x\
  |  x  \
  |   x   \
  |      x  \
  |_________x_\_____ ->a








  */

  val n = BigInt(2)
  val nbig = n.pow(100)
  val log2 = Stream.from(100).takeWhile(p => n.pow(p) < BigInt(10).pow(200 - 1)).last
  val log3 = Stream.from(100).takeWhile(p => BigInt(3).pow(p) < BigInt(10).pow(200 - 1)).last

  BigInt(2).pow(log2)
  BigInt(3).pow(log3)
  val allOnes = (0 until 200).map(x => BigInt(10).pow(x)).sum
  allOnes.toString(10).size

  represent(allOnes)

  /* Find a number with exactly 200 decimal digits, consisting only of the digits 1 and 0 with a leading 1,
     that can be represented by a sum of less than 150 summands.
  */

  def score(n: BigInt): Double = {
    // actually, how about we just do

    // If we want a 3 decimal digits we're looking at 10^2 = 100
    val diffFrom100000 = BigInt(10).pow(200 - 1) - n
    //val diffFrom111111 = allOnes - n

    /*
   println("scoring " + n)
   println("1000   " + diffFrom100000)
  */
    //println("1111   " + diffFrom111111)

    if (n > allOnes) //if (diffFrom111111 < 0)
      -1 //BigInt(10).pow(305)
    else {
      // We want diffFrom10000 to be as small as possible. Is

      val nString = n.toString(10)
      if (!(nString.head == '9' && nString.length() == 199 || nString.head == '1' && nString.length() == 200))
        // We want the highest to score highest
        nString.size / 200.0
      //0
      else {

        // want to score 1010110009999
        // but not 11019999999  // NO!!! This is still salvagable
        // want to score 999234
        if (nString.head == '1') {
          val (firstHalf, secondHalf) = nString.span(c => c == '0' || c == '1')
          if (firstHalf.last == '1') // then the next number has to be 2 or greater so we're already too high
            if (firstHalf.size == 200)
              200
            else
              firstHalf.size - firstHalf.reverse.takeWhile(p => p == '1').size // we'll count until the last 0.
          else
            firstHalf.size + secondHalf.takeWhile(_ == '9').size
        } else
          nString.takeWhile(_ == '9').size
      }
    }
  }

  println(allOnes)

  //asymp.map(x => (x, x.toBigInt > allOnes)).take(30).foreach(println)
  //asymp.map(x => (x, score(x.toBigInt))).take(200).foreach(println)

  //asymp.map(x => (x, score(x.toBigInt))).takeWhile(x => x._2 != -1).filter(_._2 > 2).sortBy(_._2)foreach(println)

  abstract class candidate
  case class SinglecandsFromLeft(a: Int, b: Int, n: BigInt) extends candidate
  case class SinglecandsFromRight(a: Int, b: Int, n: BigInt) extends candidate
  case class Paircands(a1: Int, b1: Int, a2: Int, b2: Int, n: BigInt) extends candidate

  // What about a greedy algorithm???
  // we have roughly a 400x600 grid....
  // take the one with the high score
  // decimate the grid based on that choice
  // recurse
  // actually, how about we sort them all by high score

  def greedy(depth: Int, solutionSoFar: List[(Int, Int)], n: BigInt, candidates: Set[(Int, Int)], remaining: Int): Iterator[List[(Int, Int)]] =
    {
      println("depth " + depth)
      println("n " + n)
      println("sols So far " + solutionSoFar)
      println("cand.size " + candidates.size)
      println("remaining " + remaining)
      println("score(n) " + score(n))

      if (score(n) == 200)
        Iterator(List())
      else if (solutionSoFar.size >= 150 || remaining <= 0)
        Iterator()
      else {
        val sortedCandidates = candidates.map(c => (c, n + BigInt(2).pow(c._1) * BigInt(3).pow(c._2))).filter(x => score(x._2) > score(n)).toList.sortBy(s => -score(s._2)) // highest scores first
        for {
          cand <- sortedCandidates.toIterator
          rest <- greedy(depth + 1, solutionSoFar :+ cand._1, cand._2, sortedCandidates.filter {
            // Only the ones above and to the left, ||
            // below and to the right
            // and with an improved score
            c =>
              (score(c._2) > score(n) && (c._1._2 > cand._1._2 && c._1._1 < cand._1._1 ||
                c._1._2 < cand._1._2 && c._1._1 > cand._1._1))
          }.map(c => c._1).toSet, remaining - 1)
        } yield cand._1 :: rest
      }
    }

  val initialCandidates = (for {
    exp2 <- Stream.from(0).takeWhile(p => BigInt(2).pow(p) < allOnes)
    exp3 <- Stream.from(0).takeWhile(p => BigInt(2).pow(exp2) * BigInt(3).pow(p) < allOnes)
  } yield (exp2, exp3)).toSet

  println("initialCandidates.size " + initialCandidates.size)

  val givenSolListBigInt = givenSolListString.map(s => BigInt(s))

  val candidatesMap = initialCandidates.map(p => (BigInt(2).pow(p._1) * BigInt(3).pow(p._2), p)).toMap

  val allonesSol = represent(allOnes).map(p => candidatesMap.apply(p))
  println("allonesSOl " + allonesSol)

  val givenSolListPairs = givenSolListBigInt.map(b => candidatesMap.apply(b))
  println("Here are all the solution pairs")
  givenSolListPairs.foreach(println)
  val confirmation = givenSolListPairs.map(p => BigInt(2).pow(p._1) * BigInt(3).pow(p._2)).sum
  println("confirmation")
  println(confirmation)
  println("has digits " + confirmation.toString().size)

  greedy(0, List(), 0, initialCandidates, 150).take(1).foreach(println)

  def solve(depth: Int, solutionSoFar: List[(Int, Int)], n: BigInt, threes: (Int, Int), twos: (Int, Int), twoOrThree: Boolean, remaining: Int): Iterator[List[(Int, Int)]] =
    {
      println("depth = " + depth + " n=" + n)
      println(threes + " " + twos + " " + twoOrThree)
      println("sols So far " + solutionSoFar)
      println("sols.size " + solutionSoFar.size)
      println("score = " + score(n))
      if (score(n) == 200 && solutionSoFar.size <= 150) {
        println("Solution found " + n)
        Iterator(List()) // There is a solution, and here it is: List()
      } else if (remaining == 0 || solutionSoFar.size >= 150 || (score(n) > 1 && score(n) / (solutionSoFar.size + 0.01) < 1.2))
        Iterator()
      else {
        /*  if (twoOrThree)
   {
   // Let's go up from the 2s
   */

        // Let's go right from the 3s.
        val candsFromLeftSingle = for {
          exp2 <- ((threes._1 + 1) until scala.math.min(threes._1 + 6, twos._1))
          //dummy = { println("exp2 " + exp2) }
          (exp3, newN) <- Stream.from(0).map { exp3 =>
            // println("exp3 " + exp3)
            (exp3, n + BigInt(2).pow(exp2) * BigInt(3).pow(exp3))
          }.takeWhile { x =>
            val s = score(x._2)
            //  println("x = " + x)
            //  println("s = " + s)
            s > 0
          }
        } yield SinglecandsFromLeft(exp2, exp3, newN)

        //	println("here1")
        //	println(candsFromLeftSingle)

        val ListOfhighestCandsFromLeft = for {
          exp2 <- ((threes._1 + 1) until scala.math.min(threes._1 + 4 /*6*/ , twos._1))
          //dummy = { println("exp2 " + exp2) }
          (exp2, exp3, newN) <- Stream.from(twos._2 + 1).map { exp3 =>
            //println("exp3 " + exp3)
            (exp2, exp3, n + BigInt(2).pow(exp2) * BigInt(3).pow(exp3))
          }.takeWhile { x =>
            val s = score(x._3)
            //println("x = " + x)
            //println("s = " + s)
            s > 0 && x._2 < threes._2
          }.toList.lastOption
        } yield SinglecandsFromLeft(exp2, exp3, newN)

        //  println("here2")
        //  println(ListOfhighestCandsFromLeft)

        val ListOfhighestCandsFromRight = for {
          exp3 <- ((twos._2 + 1) until scala.math.min(twos._2 + 4 /*6*/ , threes._2))
          //dummy = { println("aaa exp3 " + exp3) }
          (exp2, exp3, newN) <- Stream.from(threes._1 + 1).map { exp2 =>
            //println("aaaexp2 " + exp2)
            (exp2, exp3, n + BigInt(2).pow(exp2) * BigInt(3).pow(exp3))
          }.takeWhile { x =>

            val s = score(x._3)
            //println("x = " + x)
            //println("s = " + s)
            s > 0 && x._1 < twos._1
          }.toList.lastOption
        } yield SinglecandsFromRight(exp2, exp3, newN)

        // println("here3")
        //  println(ListOfhighestCandsFromRight)

        val pairFromLeft = ListOfhighestCandsFromLeft.combinations(2).map(x => Paircands(x(0).a, x(0).b, x(1).a, x(1).b,
          n + BigInt(2).pow(x(0).a) * BigInt(3).pow(x(0).b) + BigInt(2).pow(x(1).a) * BigInt(3).pow(x(1).b)))

        val pairFromRight = ListOfhighestCandsFromRight.combinations(2).map(x => Paircands(x(0).a, x(0).b, x(1).a, x(1).b,
          n + BigInt(2).pow(x(0).a) * BigInt(3).pow(x(0).b) + BigInt(2).pow(x(1).a) * BigInt(3).pow(x(1).b)))

        val allPairsFromLeftAndRight = for {
          c1 <- ListOfhighestCandsFromLeft
          c2 <- ListOfhighestCandsFromRight
          if (c1.b > c2.b && c1.a < c2.a) // above && to the left
        } yield Paircands(c1.a, c1.b, c2.a, c2.b, n + BigInt(2).pow(c1.a) * BigInt(3).pow(c1.b) + BigInt(2).pow(c2.a) * BigInt(3).pow(c2.b))

        //println("here4")
        //println(allPairsFromLeftAndRight)

        val bestPairs = allPairsFromLeftAndRight.sortBy(x => score(x.n)).reverse.take(5)
        // (Int, Int, BigInt),(Int,Int, BigInt), BigInt

        val bestCandsfromLeft = ListOfhighestCandsFromLeft.sortBy(x => score(x.n)).reverse.take(5)
        val bestCandsfromRight = ListOfhighestCandsFromRight.sortBy(x => score(x.n)).reverse.take(5)
        // (Int,Int,BigInt)

        //   println("bestcands " + bestCandsfromLeft)

        val bestOfBest = (bestPairs ++ bestCandsfromLeft ++ pairFromLeft ++ bestCandsfromRight ++ pairFromRight).sortBy(x => x match {
          case SinglecandsFromLeft(a, b, n)  => score(n)
          case SinglecandsFromRight(a, b, n) => score(n)
          case Paircands(a1, b1, a2, b2, n)  => score(n)
        }).reverse.take(3 /*5*/ )
        //   def solve(depth: Int, solutionSoFar: Set[(Int,Int)], n: BigInt, threes: (Int,Int), twos: (Int,Int), twoOrThree: Boolean, remaining: Int ) : Iterator[List[(Int,Int)]] =

        for {
          bestCand1 <- bestOfBest.toIterator
          rest <- bestCand1 match {
            case SinglecandsFromLeft(a, b, n)  => solve(depth + 1, solutionSoFar :+ ((a, b)), n, (a, b), twos, false, remaining - 1)
            case SinglecandsFromRight(a, b, n) => solve(depth + 1, solutionSoFar :+ ((a, b)), n, threes, (a, b), false, remaining - 1)
            case Paircands(a1, b1, a2, b2, n)  => solve(depth + 1, solutionSoFar ++ List((a1, b1), (a2, b2)), n, (a1, b1), (a2, b2), false, remaining - 2)
          }

        } yield bestCand1 match {
          case SinglecandsFromLeft(a, b, n)  => (a, b) :: rest
          case SinglecandsFromRight(a, b, n) => (a, b) :: rest
          case Paircands(a1, b1, a2, b2, n)  => List((a1, b1), (a2, b2)) ++ rest
        }
      }

    }
      */

  def main(args: Array[String]): Unit = {
    //  solve(0, List(), 0, (-1,10000), (10000,-1), false, 150).take(1).foreach(println)
  }

}