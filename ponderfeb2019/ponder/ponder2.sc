object ponder2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

   def toHexList(i: BigInt): List[Int] = i match {
        case i if (i == BigInt(0)) => Nil
        case _                     => (i % 16).toInt :: toHexList(i / 16)
      }                                           //> toHexList: (i: BigInt)List[Int]
/*
Solution found S=1165703083461682665 n = 1958318009
1 0 2 13 6 8 12 7 3 11 4 15 10 5 14 9
END SOLUTION
Solution found S=1167912712342765305 n = 1960173159
1 0 3 5 4 2 6 12 8 11 13 10 7 14 15 9
END SOLUTION
Solution found S=1171776304617085545 n = 1963412723
1 0 4 2 15 12 5 7 3 8 14 11 13 10 6 9
END SOLUTION

*/
val S = BigInt("1167912712342765305")             //> S  : scala.math.BigInt = 1167912712342765305
val n = 1960173159                                //> n  : Int = 1960173159
val n1 = 1958318009                               //> n1  : Int = 1958318009
//val Sr = SumEulerphiFromMathProblems123(n)+1
//val Sr1 = SumEulerphiFromMathProblems123(n1)+1
toHexList(n).reverse                              //> res0: List[Int] = List(7, 4, 13, 5, 13, 14, 6, 7)
toHexList(S).reverse                              //> res1: List[Int] = List(1, 0, 3, 5, 4, 2, 6, 12, 8, 11, 13, 10, 7, 14, 15, 9)
                                                  //| 
toHexList(S).size                                 //> res2: Int = 16
toHexList(S).distinct.size                        //> res3: Int = 16
toHexList(S).distinct                             //> res4: List[Int] = List(9, 15, 14, 7, 10, 13, 11, 8, 12, 6, 2, 4, 5, 3, 0, 1)
                                                  //| 


 val SumEulerphiFromMathProblems123Map = collection.mutable.Map[BigInt, BigInt]()
                                                  //> SumEulerphiFromMathProblems123Map  : scala.collection.mutable.Map[BigInt,Big
                                                  //| Int] = Map()
  def SumEulerphiFromMathProblems123Function(n: BigInt): BigInt =
    (n) * (n + 1) / 2 -
      (2.toLong to sqt(n).toLong).map(m => SumEulerphiFromMathProblems123(n / m)).sum -
      (1.toLong to sqt(n).toLong).takeWhile(d => d != n / d).map(d => (n / d - n / (d + 1)) * SumEulerphiFromMathProblems123(d)).sum
                                                  //> SumEulerphiFromMathProblems123Function: (n: BigInt)BigInt

  
  
  def SumEulerphiFromMathProblems123(n: BigInt) =
      SumEulerphiFromMathProblems123Map.getOrElseUpdate(n, SumEulerphiFromMathProblems123Function(n))
                                                  //> SumEulerphiFromMathProblems123: (n: BigInt)BigInt

 
 

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
  }                                               //> binary_search: (key: BigInt, lowerBound: BigInt, upperBound: BigInt)Option[
                                                  //| BigInt]

  def sqt(n: BigInt): BigInt = {
    var a = BigInt(1)
    var b = (n >> 5) + BigInt(8)
    while ((b - a) >= 0) {
      var mid: BigInt = (a + b) >> 1
      if (mid * mid - n > 0) b = mid - 1
      else a = mid + 1
    }
    a - 1
  }                                               //> sqt: (n: BigInt)BigInt

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
    }                                             //> exponential_search: (target: BigInt)Option[BigInt]
    
   def main(args: Array[String]) = {
     val Sr = SumEulerphiFromMathProblems123(n1)+1
    println(Sr)
    println(toHexList(n1).reverse)
println(toHexList(Sr).reverse        )
println(toHexList(Sr).size          )
println(toHexList(Sr).distinct.size)
println(toHexList(Sr).distinct)
    
    
    val base = 16
    println("starting base " + base)
    // with base = 10 sol  : Option[Long] = Some(1296348570)

// Crap! The permutations method is an order slower.
// 16! is much larger than 16^8.

    def solveWithinRange(fakeRange: IndexedSeq[Int]) = {
      println("starting ", fakeRange)
      val createN = for {
      //  firstNumber <- (r1 to r2 /*(base -1)*/ ).toIterator
        rest <- ((0 to (base - 1)) diff fakeRange /*List(1, firstNumber)*/).permutations
      } yield (fakeRange ++   /*1 +: firstNumber +: */ rest)

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
    solveWithinRange( IndexedSeq(1,12) ).foreach(println)
   // solveWithinRange( IndexedSeq(1,13,14,12) ).foreach(println)
    
    
    // Did indexseq(1 , a,  a+1, a+2) where a goes from 0 to 14, except the last is 1 14 15 13
 }                                                //> main: (args: Array[String])Unit
}