import scala.concurrent.{ Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object ponder {
  // Asymptotic sum
  // s = 3n^2 / pi^2
  // If s is a 10 digit number, then the maximum n is
  // n = sqrt(10^10 * pi^2/3)
  // nMax = 181379.93332941

  def smallestPrimeThatDividesN(n: Int): Int = {
    (2 to math.sqrt(n).toInt).find(i => n % i == 0) match {
      case Some(i) => i
      case _       => n
    }
  }

  val L = 1
  val U = 20

  val listOfSmallestPrimesThatDividesN = (L to U).map(n => (n, smallestPrimeThatDividesN(n))).toMap
  val listOfSmallestPrimesThatDividesNArray = (0 to U).map(n => smallestPrimeThatDividesN(n)).toArray

  def listOfPrimeFactorsOfN(n: Int): List[Int] = listOfSmallestPrimesThatDividesNArray(n) match {
    case p if (p != 1) => p :: listOfPrimeFactorsOfN(n / p)
    case _             => Nil
  }

  val eulerPhi = (L to U).map(n => listOfPrimeFactorsOfN(n).groupBy(identity).mapValues(_.size).map { case (p, k) => (p - 1) * math.pow(p, k - 1) }.product.toInt)

  val T = eulerPhi.scanLeft(0)(_ + _)

  // Greatest common divisor
  def gcd(n: Int, k: Int): Int = k match {
    case 0 => n
    case _ => gcd(k, n % k)
  }

  // EulerPhi via for comprehension
  for { nn <- 1 to 10 } yield {
    val nums = for {
      n <- 1 to nn
      k <- 1 to n
      if (gcd(n, k) == 1)
    } yield (n, k)
    nums.length
  }
  val eulerphi = Stream.from(1).take(181380).map(n => (1 to n).count(gcd(n, _) == 1))
  def primeStream(s: Stream[Int]): Stream[Int] =
    Stream.cons(s.head, primeStream(s.tail filter { n => n % s.head != 0 }))
  // Since we are searching for primes <= sqrt(n), where our max n = 181379, our max Primes is 425, so the 82nd prime is sufficient.
  // Which is (10^10*pi^2 / 3)^1/4.
  // Wow a fourth root!
  // We let the magic of Streams calculate only what's necessary. No need to precalculate anything to cache.
  val primes = primeStream(Stream.from(2))
  primes.take(82)
  // Find all the prime factors of a, with repetition. If a = 12, return List(2,2,3)
  def primeDecomposition(a: Int, startingFrom: Stream[Int]): List[Int] = {
    val newStream = startingFrom.dropWhile(prime => prime <= math.sqrt(a) && a % prime != 0)
    newStream.head match { // Either it found a factor or it didn't.
      case prime if (a % prime == 0) => prime :: primeDecomposition(a / prime, newStream)
      case _ if (a == 1)             => List() // We don't want a '1' to end up in our list of prime factors
      case _                         => List(a)
    }
  }

  primeDecomposition(1, primes)
  primeDecomposition(2, primes)
  primeDecomposition(3, primes)
  primeDecomposition(4, primes)
  primeDecomposition(12, primes)

  // Euler's product formula
  // phi(n) = n * product over (primes that divide n) of (1-1/prime)
  val eulerphiFromProductUsingDoubles = Stream.cons(1, Stream.from(2).map(
    n => (n * primeDecomposition(n, primes).distinct.map(p => 1 - 1.0 / p).product).toInt))
  // (1-1.0/p).product is a product of Doubles, with all the rounding errors that that entails.
  // Let's see if we can do this all in integers (get rid of the Doubles)
  // (1-1/p) = (p-1)/p.
  // Numerator = (p-1).product.
  // Denominator is p.product.
  // Since we are calculating phi(n) = n * Numerator/Denominator, n/Denominator = n / p.product will be an integer.
  // n is a product of all the primes with repetiton, and the denominator is those primes with repetitions removed, (.distinct).
  // so n/Denom is just the product of the set difference (primes - primes.distinct)
  val eulerphiFromProductUsingIntsOnly = Stream.from(1).map {
    n =>
      {
        val ps = primeDecomposition(n, primes)
        val psDistinct = ps.distinct
        val numerator = psDistinct.map(p => p - 1).product
        (ps diff psDistinct).product * numerator
      }
  }
  val eulerphiFromProductUsingIntsOnlyWithoutSets = Stream.from(1).map {
    n =>
      {
        val ps = primeDecomposition(n, primes)
        val psDistinct = ps.distinct
        val num = psDistinct.map(p => p - 1).product
        val denom = psDistinct.product
        // Do the division first or we'll definitely overflow since we're using Ints!!!
        n / denom * num
      }
  }
  val SumEulerphiFromMathProblems123Map = collection.mutable.Map[BigInt, BigInt]()
  // From https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
  // See also https://trizenx.blogspot.com/2018/11/partial-sums-of-arithmetical-functions.html
  def SumEulerphiFromMathProblems123Function(n: BigInt): BigInt =
    (n) * (n + 1) / 2 -
      (2.toLong to sqt(n).toLong).map(m => SumEulerphiFromMathProblems123(n / m)).sum -
      (1.toLong to sqt(n).toLong).takeWhile(d => d != n / d).map(d => (n / d - n / (d + 1)) * SumEulerphiFromMathProblems123(d)).sum

  /* def SumEulerphiFromMathProblems123BasicWorksFunction(n2: Int): Long =
  {
   n2.toLong*(n2+1)/2 - (2 to n2).map{m=> SumEulerphiFromMathProblems123BasicWorks(n2/m)}.sum
  }
  */

  val ba = BigInt("7023199693")
  println(ba)
  println(ba / 2)
  println(sqt(ba))
  println(sqt(ba).toLong)
  println((2.toLong to sqt(ba).toLong))
  //println((2.toLong to sqt(ba).toLong).map(m => SumEulerphiFromMathProblems123(ba / m)).sum)

  def SumEulerphiFromMathProblems123(n: BigInt) =
    {
      //      println("Called sumeupler with ", n)
      //     println("Size of Map ", SumEulerphiFromMathProblems123Map.size)
      SumEulerphiFromMathProblems123Map.getOrElseUpdate(n, SumEulerphiFromMathProblems123Function(n))
    }
  //def SumEulerphiFromMathProblems123BasicWorks(n: Int) =
  // SumEulerphiFromMathProblems123BasicWorksMap.getOrElseUpdate(n, SumEulerphiFromMathProblems123BasicWorksFunction(n))

  SumEulerphiFromMathProblems123(5)
  (2 to Math.sqrt(5).toInt).map(m => SumEulerphiFromMathProblems123(5 / m)).sum
  SumEulerphiFromMathProblems123(2)
  (1 to Math.sqrt(5).toInt).map(d => (5 / d - 5 / (d + 1)) * SumEulerphiFromMathProblems123(d)).sum

  /*SumEulerphiFromMathProblems123BasicWorks(10)

5 - (for (m <- 2 to 1) yield ( 5)).sum
(2 to 1)
(2 to 1).sum
(1 to 10).map( n=> n == Math.pow(math.sqrt(n).toInt,2))
 (1.toLong to 10).map(SumEulerphiFromMathProblems123).foreach(println)
 (1 to 10).map(SumEulerphiFromMathProblems123BasicWorks).foreach(println)

 (1.toLong to 10).map(SumEulerphiFromMathProblems123).sliding(2).map(x => x(1) - x(0)).foreach(println)
 (1 to 15).map(SumEulerphiFromMathProblems123BasicWorks).sliding(2).map(x => x(1) - x(0)).foreach(println)
*/

  eulerphi.take(15).foreach(println)
  eulerphiFromProductUsingDoubles.take(10).foreach(println)
  eulerphiFromProductUsingIntsOnly.take(10).foreach(println)
  eulerphiFromProductUsingIntsOnlyWithoutSets.take(10).foreach(println)

  // ****  PROBLEM USING DOUBLES!!!****//
  println("here1")
  eulerphi.take(1000).zip(eulerphiFromProductUsingDoubles).filter { case (a, b) => a != b }
  println("here2")

  // eulerphi.take(10000).zip(eulerphiFromProductUsingIntsOnly).find{ case (a,b) => a != b}
  println("here3")

  // Note that 10^10 is greater than what Ints can handle. So scanLeft has to start with a 0.toLong, to keep all the additions in Long.
  // _ + _ is a Long + Int => Long
  //  2147483647
  //  Int.MaxValue
  math.log10(Int.MaxValue)
  math.pow(10, 10)
  Int.MaxValue < math.pow(10, 10)
  179174 > math.sqrt(Int.MaxValue)

  999941143.toHexString

  // Luckily scanLeft prepends a 0, and zipWithIndex also starts with 0, so our indexes work out right. We know from the question that
  // "64 different values ... that only 14 people were used to label the data."
  // and we get (64,14) from our zip, so we are happy.

  //val asymptotes = solutions.map{ case (s,n) => 3.0*n*n / (math.Pi*math.Pi)}.toList
  //val absoluteDiff = solutions.zip(asymptotes).map { case (s,a) => s._1 - a  }.toList
  //val relativeDiff = solutions.zip(asymptotes).map { case (s,a) => s._1.toDouble / a  }.toList

  (1 to 3).permutations.foreach(println)
  (1 to 3).permutations.map(x => x.zipWithIndex).foreach(println)
  (1 to 3).permutations.map(x => x.zipWithIndex).map(x => x.map { case (digit, ex) => digit * math.pow(10, 2 - ex) }).foreach(println)
  (1 to 3).permutations.map(x => x.zipWithIndex).map(x => x.map { case (digit, ex) => digit * math.pow(10, 2 - ex) }.sum.toLong).foreach(println)
  val perms = (1 to 3).permutations.map(x => x.zipWithIndex).map(x => x.map { case (digit, ex) => digit * math.pow(10, 2 - ex) }.sum.toLong)
  println("here4")

  def binary_search(key: BigInt, lowerBound: BigInt, upperBound: BigInt): Option[BigInt] = {
    // println(key, lowerBound,upperBound)
    if (upperBound - lowerBound < 0)
      None
    else {
      val stab = (upperBound + lowerBound) / 2
      val stabVal = SumEulerphiFromMathProblems123(stab)
      if (stabVal == key)
        Some(stab)
      else if (stabVal < key)
        binary_search(key, lowerBound, stab - 1)
      else
        binary_search(key, stab + 1, upperBound)
    }
  }

  def sqt(n: BigInt): BigInt = {
    var a = BigInt(1)
    var b = (n >> 5) + BigInt(8)
    while ((b - a) >= 0) {
      var mid: BigInt = (a + b) >> 1
      if (mid * mid - n > 0) b = mid - 1
      else a = mid + 1
    }
    a - 1
  }

  def exponential_search(target: BigInt): Option[BigInt] =
    {
      // Are we above or below with our initial guess
      // S = 3.0*n*n / (math.Pi*math.Pi)
      // println("inside expon")
      val initialGuess = sqt((BigDecimal(target) * BigDecimal(math.Pi) * math.Pi / 3.0).toBigInt)
      //  println("inside expon2", initialGuess, target)
      val initialTarget = SumEulerphiFromMathProblems123(initialGuess)
      //  println("exp ", target, initialTarget, initialGuess)

      val tooHigh = if (initialTarget > initialTarget) -1 else 1

      val bound = Stream.iterate(1.toLong)(_ * 2)
      val b2 = bound.find(b => tooHigh * SumEulerphiFromMathProblems123(tooHigh * b + initialGuess) > tooHigh * target) match {
        case Some(b) => b
        case _       => 0
      }
      //  println(tooHigh,b2)
      //  println(SumEulerphiFromMathProblems123(tooHigh *b2  + initialGuess))

      if (tooHigh == 1)
        binary_search(target, initialGuess + b2 / 2, initialGuess + b2)
      else
        binary_search(target, initialGuess - b2, initialGuess - b2 / 2)
    }

  println("pre1")

  println("pre2", exponential_search(123))

  println("pre3", exponential_search(1296348570))
  //println("pre4", exponential_search(BigInt("14993103655465041135")))

  println("here5")

  def main(args: Array[String]) = {

    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
    }
    //eulerphiFromProductUsingIntsOnly            Elapsed time:       851,858,435ns , num Solutions 48
    //eulerphiFromProductUsingDoubles             Elapsed time:       610,643,474ns , num Solutions 39
    //eulerphi                                    Elapsed time: 1,268.523,253,119ns , num Solutions 48. 20 Minutes
    //eulerphiFromProductUsingIntsOnlyWithoutSets Elapsed time:       388,990,314ns , num Solutions 48
    //SumEulerphiFromMathProblems123              Elapsed time:     8.532,843,913ns , num Solutions 48 // Hmmm, this was supposed to be the fastest
    //SumEulerphiFromMathProblems123BasicWorks    Elapsed time:   745.527,108,964ns , num Solutions 48, 12 minutes
    time {

      def toHexList(i: BigInt): List[Int] = i match {
        case i if (i == BigInt(0)) => Nil
        case _                     => (i % 16).toInt :: toHexList(i / 16)
      }

      //val startingValue = sqt((BigDecimal(16).pow(15) * BigDecimal(math.Pi) * math.Pi / 3.0).toBigInt)
      val startingValue = 1955982680
      
      //val sumeuler = eulerphiFromProductUsingIntsOnlyWithoutSets.scanLeft(0.toLong)(_ + _).zipWithIndex
      val sumeuler = Stream.iterate(startingValue){ last =>
       val lastS = toHexList(SumEulerphiFromMathProblems123(last) + 1).reverse
  		 val bound = Stream.iterate(1)(_ * 2)
  		 val nextS = bound.map(b=> (b,toHexList(SumEulerphiFromMathProblems123(last+b) + 1).reverse))
  		 val commonPrefix = nextS.map{ case (b,ns) => (b,ns, ns.zip(lastS).takeWhile(x=> x._1 == x._2)   )}
       // Find the first one that has no duplicates in the common prefix
       val lastCommonPrefix = commonPrefix.takeWhile(x => x._3.size > 0 && x._3.size != x._3.distinct.size).lastOption
       //println("h2", last, lastS, nextS, commonPrefix)
       lastCommonPrefix match {
        case Some(x) => {
         //println("found ", last, x._1)
         last+ x._1
        }
        case None => {
        // println("didn't find ", last)
         last + 1
        }
       }
	    }.map(x => (SumEulerphiFromMathProblems123(x) + 1, x)).zipWithIndex
      //val sumeuler = Stream.from(startingValue).map(x => (SumEulerphiFromMathProblems123(x) + 1, x))
      //val sumeuler2 = Stream.from(57355).map(x => (SumEulerphiFromMathProblems123(x),x))

      // sumeuler.take(15).foreach(println)
      // sumeuler.find{ case(nInt,i) => nInt > math.pow(10,10)}
      // res4: Option[(Long, Int)] = Some((10000016400,181380))

      // sumeuler.find{ case(nInt,i) => nInt > math.pow(10,11)}
      // res3: Option[(Long, Int)] = Some((100000195152,573574))

      sumeuler.filter(_._2 % 1/*00*/ == 0).foreach(x=>println(toHexList(x._1._1).reverse , toHexList(x._1._2).reverse , x._2) )
      // The takeWhile stops the stream when the sums contains more than 10 digits. No more solutions will be found due to the pigeonhole principle.
      val solutions = sumeuler.takeWhile(_._1._1 < BigInt(16).pow(16)).filter {
        case ((nInt, i),_) =>
          //nInt.toString().distinct.size == 10
          toHexList(nInt).distinct.size == 16
      }
      //val solutions2 = sumeuler2.takeWhile( _._1 < math.pow(10,10) ).filter{ case(nInt, i) =>
      // nInt.toString().distinct.size == 10
      //}

      printf("Number of solutions found is %d\n", solutions.length)
      // https://www.wolframalpha.com/input/?i=sum+from+n%3D1+to+65305+of+eulerphi(n)
      // sum_(n=1)^65305 ϕ(n) = 1296348570
      // sum_(n=1)^179174 ϕ(n) = 9758312640
      // Sum[EulerPhi[n], {n, 1, 179174}] // Wolfram Language code

      solutions.foreach(x => printf("Number of possible scores is %d, when mumber of people is %d\n", x._1, x._2))

    }

    /*
    val base = 16
    println("starting base " + base)
    // with base = 10 sol  : Option[Long] = Some(1296348570)

    def solveWithinRange(r1: Int, r2: Int) = {
      println("starting ", r1, " ", r2)
      val fakerange = IndexedSeq(1, 0, 2, 3, 4, 5)
      val createN = for {
        //  firstNumber <- (r1 to r2 /*(base -1)*/ ).toIterator
        rest <- ((0 to (base - 1)) diff fakerange /*List(1, firstNumber)*/ ).permutations
      } yield (fakerange ++ /*1 +: firstNumber +: */ rest)

      createN.zipWithIndex.map {
        case (x, i) =>
          if (i % 100000 == 0)
            println("here10", x)
          x.zipWithIndex
      }.map(x => x.map {
        case (digit, ex) =>
          //     println(digit, ex, BigInt(digit) * BigInt(base).pow(base - 1 - ex))
          BigInt(digit) * BigInt(base).pow(base - 1 - ex)
      }.sum)
        .filter { x =>
          //   println("inside filter ", x)
          exponential_search(x) match {
            case Some(b) => {
              println("Solution found!!!", b)
              true
            }
            case _ => {
              //     println(" didn't find sol")
              false
            }
          }
        }
    }
    solveWithinRange(2, 4).foreach(println)
*/
    /*    def sol1 = Future {
      solveWithinRange(0, 3)
    }
    def sol2 = Future {
      solveWithinRange(4, 7)
    }
    def sol3 = Future {
      solveWithinRange(8, 11)
    }
    def sol4 = Future {
      solveWithinRange(12, 15)
    }

    println("future1")
    sol1.onComplete {
      case Success(value) =>
        {
          println("future1a", value.size)
          value.foreach(x => println("sol1 " + x))
        }
      case _ => println("Nothing found in range 1")
    }
    sol2.onComplete {
      case Success(value) => value.foreach(x => println("sol2 " + x))
      case _              => println("Nothing found in range 2")
    }
    sol3.onComplete {
      case Success(value) => value.foreach(x => println("sol3 " + x))
      case _              => println("Nothing found in range 3")
    }
    sol4.onComplete {
      case Success(value) => value.foreach(x => println("sol4 " + x))
      case _              => println("Nothing found in range 4")
    }

    def sleep(time: Long) { Thread.sleep(time) }
    // important: keep the jvm from shutting down
    sleep(1000000000)
  */
  } // Main
} // Object Ponder