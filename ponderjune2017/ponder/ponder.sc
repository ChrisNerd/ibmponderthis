import java.util.Calendar
/*
(Solution Found,List(0, 3, 9, 7, 7, 9, 9))
(Solution Found,List(0, 7, 6, 5, 5, 3, 3))
*/
object ponder {
 val i = List(2,3,7)                              //> i  : List[Int] = List(2, 3, 7)
 val (i1,i2) = i.splitAt(2)                       //> i1  : List[Int] = List(2, 3)
                                                  //| i2  : List[Int] = List(7)
 val a = i1(0)                                    //> a  : Int = 2
 val b = i1(1)                                    //> b  : Int = 3

 // All the outputs possible using +-*/ with just two inputs.
 // If the second input is 0, we avoid dividing by 0, and we might as well skip adding or subtracting 0.
 def basicOps(a: Double, b: Double): Set[Double] = b match {
  case 0 => Set(a,0,-a)
  case _ => Set(a+b, a-b, a*b, a/b,
               -a+b,-a-b,-a*b,-a/b)
 }                                                //> basicOps: (a: Double, b: Double)Set[Double]
 
 // Given a list of Ints, output a Set of Doubles of all the solutions to all the possible equations.
 // With just a single Int, we can return the unary -, as well as the Int itself.
 // With just two Ints, use the above basicOps function.
 // With 3 or more Ints, we split (i.e. bracket) it first (1)(2 3 4...), then (1 2)(3 4...) etc.
 //  Recursively evaluate all the solutions to each half.
 //  Take all the results of all the pairs (yield), then put them into one huge list (flatten), then remove duplicates (toSet) for efficiency
 def allOutputs(inList: List[Int]): Set[Double] = inList.size match {
  case 1 => Set(inList(0), -inList(0))
  case 2 => basicOps(inList(0),inList(1))
  case _ =>
   (
   for {
    splitPos <- 1 to inList.size - 1
    firstVal  <- allOutputs( inList.take(splitPos) )
    secondVal <- allOutputs( inList.drop(splitPos) )
   }
   yield basicOps(firstVal,secondVal)
   ).flatten.toSet
 }                                                //> allOutputs: (inList: List[Int])Set[Double]
 
 val allVals = allOutputs(List(2,3,7))            //> allVals  : Set[Double] = Set(0.14285714285714285, -0.14285714285714285, 42.
                                                  //| 0, -7.0, -8.0, 20.0, -35.0, -12.0, 1.0, -0.2, 0.2, 6.0, -0.7142857142857143
                                                  //| , -0.09523809523809523, 0.7142857142857143, 0.09523809523809523, -23.0, -19
                                                  //| .0, 13.0, 2.0, -7.666666666666667, 7.666666666666667, -42.0, -2.42857142857
                                                  //| 14284, 2.4285714285714284, 12.0, -0.5, 0.5, -20.0, 7.0, -0.8571428571428571
                                                  //| , 0.8571428571428571, 35.0, -1.5714285714285714, 1.5714285714285714, -1.0, 
                                                  //| 4.666666666666667, -4.666666666666667, -4.666666666666666, 4.66666666666666
                                                  //| 6, 23.0, 8.0, -6.0, -13.0, 19.0, 6.333333333333333, -6.333333333333333, -2.
                                                  //| 0)
 // Is 100 amoung the set
 // Worried about rounding errors, less concerned about efficiency
 val meetsGoalCache = collection.mutable.Map[List[Int],Boolean]()
                                                  //> meetsGoalCache  : scala.collection.mutable.Map[List[Int],Boolean] = Map()

 def meetsGoal(i: List[Int]) =  meetsGoalCache.getOrElseUpdate(i, allOutputs(i).exists { x => math.abs(100-x) < .0001 })
                                                  //> meetsGoal: (i: List[Int])Boolean
 
 val fin2 = List(7,9)                             //> fin2  : List[Int] = List(7, 9)
 val ftest2 = allOutputs(fin2)                    //> ftest2  : Set[Double] = Set(-63.0, 2.0, -0.7777777777777778, 0.777777777777
                                                  //| 7778, 63.0, 16.0, -16.0, -2.0)
 val ftest2s = ftest2.size                        //> ftest2s  : Int = 8
 val ftestB2 = meetsGoal(fin2)                    //> ftestB2  : Boolean = false
 
 val ftest3 = allOutputs(List(1,7, 9))            //> ftest3  : Set[Double] = Set(-64.0, 1.2857142857142856, -1.2857142857142856,
                                                  //|  -54.0, -3.0, -72.0, 8.857142857142858, -8.857142857142858, 1.0, -63.0, -0.
                                                  //| 0625, 0.0625, 0.6666666666666666, -0.6666666666666666, -62.0, 2.0, -15.0, 6
                                                  //| 4.0, 17.0, 54.0, -0.5, 0.5, 0.8888888888888888, -0.7777777777777778, -0.888
                                                  //| 8888888888888, 0.7777777777777778, -1.7777777777777777, 1.7777777777777777,
                                                  //|  0.2222222222222222, -0.2222222222222222, 3.0, 0.015873015873015872, -0.015
                                                  //| 873015873015872, 63.0, -1.0, 16.0, 72.0, -16.0, -2.0, -17.0, 15.0, 62.0, -9
                                                  //| .142857142857142, 9.142857142857142)
 val ftest3s = ftest3.size                        //> ftest3s  : Int = 44
 val ftest4 = allOutputs(List(2, 1, 7, 9))        //> ftest4  : Set[Double] = Set(3.857142857142857, -3.857142857142857, -32.0, -
                                                  //| 66.0, -64.0, 1.5555555555555558, -1.5555555555555558, 0.0, 1.28571428571428
                                                  //| 56, -1.2857142857142856, 5.0, -128.0, -8.571428571428571, 3.777777777777777
                                                  //| 7, 7.142857142857142, 56.0, 8.571428571428571, -3.7777777777777777, -54.0, 
                                                  //| -7.142857142857142, -0.125, 0.125, -7.0, -0.22580645161290322, -81.0, 0.225
                                                  //| 80645161290322, 25.0, -0.11764705882352941, 52.0, 14.0, 0.11764705882352941
                                                  //| , 19.285714285714285, -19.285714285714285, 1.5, 0.13333333333333333, -1.5, 
                                                  //| -108.0, -0.13333333333333333, -9.25, 189.0, -3.0, 9.25, -18.0, 17.714285714
                                                  //| 285715, -17.714285714285715, -0.20634920634920637, 0.20634920634920637, -11
                                                  //| .142857142857142, 11.142857142857142, -12.0, 1.125, -1.125, 61.0, -72.0, 8.
                                                  //| 857142857142858, -8.857142857142858, 1.0, 0.037037037037037035, 74.0, -0.03
                                                  //| 7037037037037035, -63.0, 0.5555555555555556, -0.5555555555555556, 6.0, 60.0
                                                  //| , 0.7142857142857144, 3
                                                  //| Output exceeds cutoff limit.
 val ftest4s = ftest4.size                        //> ftest4s  : Int = 233
 val ftest5 = allOutputs(List( 3, 2, 1, 7, 9))    //> ftest5  : Set[Double] = Set(-0.0925925925925926, 69.0, 3.857142857142857, -
                                                  //| 0.9444444444444444, -3.857142857142857, -32.0, 0.9444444444444444, -11.7142
                                                  //| 85714285715, 0.2763157894736842, 53.142857142857146, -0.04054054054054054, 
                                                  //| 11.714285714285715, -66.0, -64.0, -198.0, 0.04054054054054054, -53.14285714
                                                  //| 2857146, -14.538461538461537, -3.888888888888889, -1.4881889763779528, 14.5
                                                  //| 38461538461537, -0.2763157894736842, 3.888888888888889, -71.0, 0.0, -49.0, 
                                                  //| 1.2857142857142856, -8.833333333333334, 0.0925925925925926, -1.285714285714
                                                  //| 2856, 8.833333333333334, 1.4881889763779528, -1.2777777777777777, 5.0625, 1
                                                  //| 0.285714285714285, -0.42857142857142855, -7.777777777777778, -27.0, -5.0625
                                                  //| , 0.14285714285714285, 7.777777777777778, -2.984126984126984, -1.875, 27.42
                                                  //| 8571428571427, 2.984126984126984, 1.875, -27.428571428571427, -6.75, -52.71
                                                  //| 428571428571, 5.0, 6.75, 0.18253968253968253, 52.71428571428571, -0.1825396
                                                  //| 8253968253, -0.14285714
                                                  //| Output exceeds cutoff limit.
 val ftest5s = ftest5.size                        //> ftest5s  : Int = 1235
 
 val ftest6 = allOutputs(List( 4, 3, 2, 1, 7, 9)) //> ftest6  : Set[Double] = Set(-0.0925925925925926, 0.07235142118863049, 42.28
                                                  //| 571428571428, 69.0, 3.857142857142857, 187.66666666666666, -13.214285714285
                                                  //| 714, -1.03125, 3.764705882352941, 0.04597701149425287, 2.4761904761904763, 
                                                  //| -53.857142857142854, -34.285714285714285, -7.037037037037037, -5.9259259259
                                                  //| 25925, 1.259259259259259, -10.61904761904762, 6.146341463414634, 1.54545454
                                                  //| 54545459, -0.9444444444444444, -42.42857142857143, -7.25, -0.92307692307692
                                                  //| 29, -0.07235142118863049, -3.857142857142857, 13.214285714285714, -3.764705
                                                  //| 882352941, 15.42857142857143, 53.857142857142854, -2.4761904761904763, 38.6
                                                  //| 99999999999996, 7.037037037037037, 34.285714285714285, 5.925925925925925, -
                                                  //| 8.555555555555555, -81.33333333333333, -32.0, 4.069444444444445, -0.6736842
                                                  //| 105263158, -3.6875, -1.5454545454545459, 0.9444444444444444, 7.25, -11.7142
                                                  //| 85714285715, -14.25, -39.71428571428571, 138.0, -0.16666666666666674, 25.28
                                                  //| 5714285714285, -0.27343
                                                  //| Output exceeds cutoff limit.
 val ftest6s = ftest6.size                        //> ftest6s  : Int = 6657
 val fin7 = List( 5, 4, 3, 2, 1, 7, 9)            //> fin7  : List[Int] = List(5, 4, 3, 2, 1, 7, 9)
 val ftest7 = allOutputs(fin7)                    //> ftest7  : Set[Double] = Set(-0.0925925925925926, -6.301587301587301, -2.916
                                                  //| 6666666666647, -166.66666666666669, -180.14285714285717, 184.99999999999997
                                                  //| , 35.18518518518518, -0.8974358974358975, 49.736842105263165, 645.0, 0.3086
                                                  //| 41975308642, -188.66666666666666, -0.42028985507246375, 0.9682539682539681,
                                                  //|  -4.253968253968254, -9.04054054054054, -1.3888888888888893, 0.027173913043
                                                  //| 47826, 0.3375, -257.5, -17.321428571428573, 24.555555555555554, -1.03278688
                                                  //| 52459017, -0.1258992805755396, 3.861111111111111, 69.0, -0.1649484536082474
                                                  //| 2, 0.6197916666666666, 3.857142857142857, -4.090909090909092, -1188.0, 120.
                                                  //| 42857142857142, 1.6470588235294117, 16.174603174603174, -2.4603174603174605
                                                  //| , 17.5625, 79.75, -10.333333333333332, 7.15, -4.357142857142858, -13.214285
                                                  //| 714285714, -67.28571428571429, 252.00000000000003, -1.03125, 63.35714285714
                                                  //| 2854, 0.5800000000000001, 44.25, 0.41414141414141414, 8.942307692307693, 5.
                                                  //| 140625, 0.5079365079365
                                                  //| Output exceeds cutoff limit.
 val ftest7s = ftest7.size                        //> ftest7s  : Int = 38653
 val ftestB7 = meetsGoal(fin7)                    //> ftestB7  : Boolean = true
 
 // Brute force primality test, doesn't take too long
 // Could have used Sieve of Eratosthenes if we really want to save 30 seconds.
 def isPrime(n:Int) = (2 to math.sqrt(n).toInt) forall (n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean

 // Given a 7 digit number, return a list of the 63 (7 digit) numbers that can be obtained by changing any given digit.
 def allNeibours(inList: List[Int]): Seq[List[Int]] =
  for {
   i <- (0 until inList.size)
   replaceWith <- (0 to 9).filter( _ != inList(i) )
  } yield inList.updated(i, replaceWith)          //> allNeibours: (inList: List[Int])Seq[List[Int]]
 
 val sol1 = List(0, 3, 9, 7, 7, 9, 9)             //> sol1  : List[Int] = List(0, 3, 9, 7, 7, 9, 9)
 val sol2 = List(0, 7, 6, 5, 5, 3, 3)             //> sol2  : List[Int] = List(0, 7, 6, 5, 5, 3, 3)
 val isSol1aSolution = !meetsGoal(sol1) && allNeibours(sol1).count{ meetsGoal } == 62
                                                  //> isSol1aSolution  : Boolean = true
 val isSol2aSolution = !meetsGoal(sol2) && allNeibours(sol2).count{ meetsGoal } == 62
                                                  //> isSol2aSolution  : Boolean = true
  
def main(args: Array[String]) = {
 // Generates all the prime numbers with 7 digits or less.
 // The last line 0 pads them and splits the number into a List of single digits of length 7.
 val allLists = for {
  n <- 0 to math.pow(10,7).toInt-1
  if isPrime(n)
 }
 yield "%07d".format(n).map(_.asDigit).toList

 // Should return Pi(10^7) = 664579, but returns 664581 because it counts 0 and 1 as prime.
 // But 664,000 isn't too bad. We go through about 50 / second so the total time is around 3.5 hours.
 println(allLists.size)
 allNeibours(List(5)).size
 allNeibours(List(5,6)).size
  
 //Find a seven-digit number that is not solvable, but is made solvable by 62 of the 63 single-digit changes.
 var counter = 0
 val sol =  allLists.par.filter {
  inList =>
  {
   if (counter % 10000 == 0)
   {
    println(Calendar.getInstance().getTime())
    println(inList)
    println(counter)
   }
   counter = counter +1
   // Find a seven-digit number that is not solvable,...
   ! meetsGoal(inList)
  } &&
  {
  /*
   println(meetsGoalCache.size)
   var neighbourFails = 0
   var neighbourCount = 0
   // but is made solvable by 62 of the 63 single-digit changes.
   // In other words, only 1 neighbour can fail, and exactly 1 neighbour must fail.
   // Doing it with mutable variables allows us to short circuit
   while (neighbourFails < 2 && neighbourCount < 63)
   {
    val y = allNeibours(inList)(neighbourCount)
    if (! meetsGoal(y) )
     neighbourFails = neighbourFails+1
    neighbourCount = neighbourCount+1
   }
   if (neighbourFails ==1)
    println("Solution Found", inList)
   neighbourFails == 1
*/

/*
   // This looks ***waaaayyyy*** better using purely functional code, unfortunately, it is way less effecient since
   // it doesn't short circuit after the second failure. It goes through all 63 neighbours.
   allNeibours(inList).count{ meetsGoal } == 62
*/

 // Hmmm this looks not too bad using immutable variables
  /*
  !allNeibours(inList).dropWhile { !meetsGoal }.drop(1).exists{ !meetsGoal }
 // Except if dropWhile never fails, then we get an empty list .drop(1), which would be a seg fault.
 // How about if we split it.
*/
   val firstPart = allNeibours(inList).dropWhile { meetsGoal }
   if (firstPart == List())
    false // all 63 met the goal
   else
   {
    if (firstPart.drop(1).forall{ meetsGoal }) // Do all the remaining neighbours meet the goal?
    {
     println("Solution Found", inList)
     true
    }
    else
     false
    }
   }
  }
  println("Program done. All solutions:")
  sol.foreach{println}
  
  // Verify the solutions
  val sol1 = List(0, 3, 9, 7, 7, 9, 9)
  val sol2 = List(0, 7, 6, 5, 5, 3, 3)

  val isSol1aSolution = ! meetsGoal(sol1) && allNeibours(sol1).count{ meetsGoal } == 62
  val isSol2aSolution = ! meetsGoal(sol2) && allNeibours(sol2).count{ meetsGoal } == 62

  println(isSol1aSolution)
  println(isSol2aSolution)
  
  
 }                                                //> main: (args: Array[String])Unit
}