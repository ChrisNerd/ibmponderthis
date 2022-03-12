object ponder {

  def minor(A : List[List[Int]], i: Int) = (A.take(i) ::: A.drop(i+1)).map(_.tail)
                                                  //> minor: (A: List[List[Int]], i: Int)List[List[Int]]

  def determinant(A : List[List[Int]]): Int = A.size match {
  case 1 => A.head.head
  
  case _ => ( for { a <- A.zipWithIndex }
  yield ( (if (a._2 % 2 == 0) 1 else -1 ) * a._1.head * determinant(minor(A,a._2))
  )).sum
   
 }                                                //> determinant: (A: List[List[Int]])Int
  val t = List( List(1 , 2), List(3,4))           //> t  : List[List[Int]] = List(List(1, 2), List(3, 4))
  determinant(t)                                  //> res0: Int = -2
  
  // Now make a random 9x9 matrix with 9 of each digit
  val M = (1 to 9).toList.map(x => (1 to 9).toList)
                                                  //> M  : List[List[Int]] = List(List(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4
                                                  //| , 5, 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 7,
                                                  //|  8, 9), List(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 7, 8, 9), Li
                                                  //| st(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3
                                                  //| , 4, 5, 6, 7, 8, 9))

  val scoreToBeat = 0x3704d007                    //> scoreToBeat  : Int = 923062279
  val (initialTemperature, finalTemperature, coolingRate) = (200.0, 0.1, 0.05)
                                                  //> initialTemperature  : Double = 200.0
                                                  //| finalTemperature  : Double = 0.1
                                                  //| coolingRate  : Double = 0.05
  determinant(M)                                  //> res1: Int = 0
  def createNeighbour( in : List[List[Int]]): List[List[Int]] = {
   val x1 = util.Random.nextInt(9)
   val y1 = util.Random.nextInt(9)
   val x2 = util.Random.nextInt(9)
   val y2 = util.Random.nextInt(9)
   val z1 = in(x1)(y1)
   val z2 = in(x2)(y2)
   val temp1 = in.updated(x1, in(x1).updated(y1,z2))
   temp1.updated(x2, temp1(x2).updated(y2,z1))
  }                                               //> createNeighbour: (in: List[List[Int]])List[List[Int]]
  
 val neighM = createNeighbour(M)                  //> neighM  : List[List[Int]] = List(List(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2
                                                  //| , 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 3, 8, 9), List(1, 2, 7, 4, 5
                                                  //| , 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 7, 8
                                                  //| , 9), List(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4, 5, 6, 7, 8, 9), Lis
                                                  //| t(1, 2, 3, 4, 5, 6, 7, 8, 9))
 neighM.flatten.groupBy(identity).mapValues(_.size)
                                                  //> res2: scala.collection.immutable.Map[Int,Int] = Map(5 -> 9, 1 -> 9, 6 -> 9,
                                                  //|  9 -> 9, 2 -> 9, 7 -> 9, 3 -> 9, 8 -> 9, 4 -> 9)
                                                  
val s =  List(List(4, 6, 3, 6, 9, 7, 1, 7, 8), List(3, 6, 9, 1, 2, 8, 8, 8, 6), List(4, 1, 6, 1, 8, 1, 8, 5, 7), List(9, 2, 9, 4, 5, 9, 5, 2, 2),
 List(1, 1, 2, 8, 3, 6, 9, 6, 3), List(4, 5, 9, 8, 3, 2, 7, 1, 7), List(9, 1, 5, 6, 1, 3, 3, 8, 7), List(2, 5, 9, 7, 6, 4, 3, 8, 1), List(8, 9, 1, 4, 4, 3, 7, 4, 2))
                                                  //> s  : List[List[Int]] = List(List(4, 6, 3, 6, 9, 7, 1, 7, 8), List(3, 6, 9, 
                                                  //| 1, 2, 8, 8, 8, 6), List(4, 1, 6, 1, 8, 1, 8, 5, 7), List(9, 2, 9, 4, 5, 9, 
                                                  //| 5, 2, 2), List(1, 1, 2, 8, 3, 6, 9, 6, 3), List(4, 5, 9, 8, 3, 2, 7, 1, 7),
                                                  //|  List(9, 1, 5, 6, 1, 3, 3, 8, 7), List(2, 5, 9, 7, 6, 4, 3, 8, 1), List(8, 
                                                  //| 9, 1, 4, 4, 3, 7, 4, 2))
 
 determinant(s)                                   //> res3: Int = 943564998
 determinant(s) > 0x3704d007                      //> res4: Boolean = true

 s.flatten.groupBy(identity).mapValues(_.size)    //> res5: scala.collection.immutable.Map[Int,Int] = Map(5 -> 6, 1 -> 12, 6 -> 9
                                                  //| , 9 -> 10, 2 -> 8, 7 -> 8, 3 -> 9, 8 -> 11, 4 -> 8)
  
  
  // Then simulate anneal it, with random swaps.
  def simulatedAnnealing( best: List[List[Int]], temp:Double): List[List[Int]] = {
  println("Temp " + temp)
 if (temp > finalTemperature) {
  val currentEnergy = determinant(best)
  val neighbour = createNeighbour( best )
  val neighbourEnergy = determinant(neighbour)
  if (neighbourEnergy > scoreToBeat)
  {
  println("Solution!!!")
   println(neighbour)
   neighbour
  }
  else
  {   // Decide if we should accept the neighbour
   val accept = (math.exp((neighbourEnergy - currentEnergy)/temp) > math.random)
   simulatedAnnealing( if (accept) {
    // println("\nScore: " + neighbourEnergy, " Temperature: "+ temp)
     neighbour
   } else best, (1-coolingRate)*temp)
  }
 } else best
}                                                 //> simulatedAnnealing: (best: List[List[Int]], temp: Double)List[List[Int]]
  
  
  determinant(List(List(7, 7, 3, 6, 5, 3, 1, 2, 9), List(6, 1,
 7, 5, 9, 3, 4, 9, 6), List(1, 5, 9, 9, 1, 1, 5, 7, 6), List(4, 3, 5, 1, 5,
1, 8, 4, 9), List(2, 7, 7, 4, 7, 8, 5, 3, 2), List(8, 8, 1, 4, 4, 4, 7, 9,
2), List(2, 4, 3, 3, 2, 9, 4, 7, 8), List(2, 3, 1, 9, 9, 8, 8, 3, 5), List(
9, 2, 7, 8, 1, 6, 8, 2, 6)))
  
 
 
 def main(args: Array[String]) = {
 Stream.from(1).takeWhile{p =>
  //if (p%100 == 0)
   println(p)

  val best = simulatedAnnealing(M, initialTemperature)
                                                  
 determinant(best)
 best.flatten.groupBy(identity).mapValues(_.size)
 determinant(best) < 0x3704d007
 }
 
 }
 
  }