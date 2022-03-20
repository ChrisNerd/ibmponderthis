object ponder {

  def listOfInts(minel: Int, maxel: Int, n: Int, k: Int): Stream[List[Int]] = {
    if (k == n)
      Stream(List())
    else {
      val a = for {
        i <- (minel + 1) to (maxel - (n - k) + 1)
        row <- listOfInts(i, maxel, n, k + 1)
      } yield i :: row
      a.toStream
    }
  }
  
  
  def listOfInts2(minel: Int, maxel: Int, n: Int, k: Int): Stream[List[Int]] = {
    if (k == n)
      Stream(List())
    else {
      val a = for {
        i <- (minel + 1) to (minel + 11)
        row <- listOfInts2(i, maxel, n, k + 1)
      } yield i :: row
      a.toStream
    }
  }
    
  //  val S1Stream = listOfInts(0, 13, 6, 0)
  //  val S2Stream = listOfInts(0, 23, 6, 0)

  
val S1Stream = listOfInts2(1,15,5,0)
val S2Stream = listOfInts2(2, 15,5,0)


  /*for {
    S1 <- S1Stream
    S2 <- S2Stream
  } {
    val products = (for {
      s1 <- 1 :: S1
      //s1 <- 1 :: S1
      s2 <- 2 :: S2
    } yield (s1 * s2)).toSet

    val coveringSet = products ++ (products map (_ + 1)) ++ (products map (_ - 1))

    val remainders = (1 to 57).toSet -- coveringSet

    if (remainders.size == 0) {
      println("S1= ",S1)
      println("S2= ",S2)
    }

  }
*/

  val (initialTemperature, coolingRate) = (2.0, 0.001)

  case class City(x: Int, y: Int)

  def distance(a: City, b: City) = math.sqrt(List((a.x - b.x), (a.y - b.y)).map(x => x * x).sum)

  def mkTour(tour: Seq[City]) = {
    val (i, j) = (util.Random.nextInt(tour.size), util.Random.nextInt(tour.size))
    tour
      .patch(i, Seq(tour(j)), 1)
      .patch(j, Seq(tour(i)), 1)
  }

  def length(tour: Seq[City]) = tour.foldLeft(0.0, tour.head)((a, b) => (a._1 + distance(a._2, b), b))._1

  def createNeighbour(s1: List[Int], s2: List[Int]) = {
    val r = util.Random.nextInt(24)
    //0 to 11 are subtract
    //12 to 23 are add
    val addOrSubtract = 2 * (r / 12) - 1

    //000000,111111,222222,333333...
    val set1or2 = (r / 6) % 2

    val elementInSet = r % 6

    if (set1or2 == 1)
      (s1.updated(elementInSet, s1(elementInSet) + addOrSubtract), s2)
    else
      (s1, s2.updated(elementInSet, s2(elementInSet) + addOrSubtract))
  }


def remainders(S1: List[Int], S2: List[Int]): Set[Int] =
{
      val products = (for {
        s1 <- S1
        s2 <- S2
      } yield (s1 * s2)).toSet

      val coveringSet = products ++ (products map (_ + 1)) ++ (products map (_ - 1))
    (1 to 59).toSet -- coveringSet
}

def numRemainedrs(S1: List[Int], S2: List[Int]) =
{
   remainders(S1, S2).size
}


  def largestRun(S1: List[Int], S2: List[Int]) =
    {
      val products = (for {
        s1 <- S1
        s2 <- S2
      } yield (s1 * s2)).toSet

      val coveringSet = products ++ (products map (_ + 1)) ++ (products map (_ - 1))

      val coveringList = coveringSet.toList.sorted
if (coveringList.head == 1) {
      val d = coveringList.sliding(2).toList indexWhere { case List(a, b) => b > a + 1 }
      if (d == -1)
      coveringList.last
      else
      coveringList(d)
      }
      else
      0
      /*val diffs = coveringList.tail zip( coveringList drop 1)
	val diffs2 = diffs map (x=> x._1 - x._2)
	diffs2 indexWhere(_ > 1)*/

    }

  // If the new solution is better, accept it, else compute acceptance probability
  def acceptanceProbability(oldEnergy: Double, newEnergy: Double, temperature: Double) = {
    math.exp(  (oldEnergy - newEnergy) / temperature)
  }

  def compute(bestS1: List[Int], bestS2: List[Int], temp: Double): (List[Int], List[Int]) = {
    if (temp > .00001) {
      val newSolution = createNeighbour(bestS1, bestS2)
    //  println("NewSolution", newSolution._1, newSolution._2)
      val currentEnergy = numRemainedrs(bestS1, bestS2)
//      val currentEnergy = largestRun(bestS1, bestS2)
    //  println("currentEnergy", currentEnergy)
  
  val neighbourEnergy = numRemainedrs(newSolution._1, newSolution._2)
  //    val neighbourEnergy = largestRun(newSolution._1, newSolution._2)
    //if (neighbourEnergy > 56)
    if (neighbourEnergy == 0 )
    { println("neighbourEnergy", neighbourEnergy, newSolution._1, newSolution._2 ) }
      // Decide if we should accept the neighbour
      val acceptProb = acceptanceProbability(currentEnergy, neighbourEnergy, temp)
    //  println("accept prob", acceptProb)
      val accept = (acceptProb> math.random) /*&&
                       (length(newSolution) < length(best))*/
      if (accept) {
    //    println("Accepted Energy: ", neighbourEnergy, "Temperature: ", temp)
        compute(newSolution._1, newSolution._2, (1 - coolingRate) * temp)
      } else {
    //  println("Did not accepted Energy: ", neighbourEnergy, "Temperature: ", temp)
      compute(bestS1, bestS2, (1 - coolingRate) * temp)
      }
    } else (bestS1, bestS2)
  }

  def main(args: Array[String]) = {
    for (iii <- 1 to 100) {
      val best = compute(List(1, 19, 3+util.Random.nextInt(5), 4+util.Random.nextInt(8),
      5+util.Random.nextInt(11),
      7+util.Random.nextInt(15)),
      List(2,
      3,
      4+util.Random.nextInt(5),
      5+util.Random.nextInt(5),
      10+util.Random.nextInt(5),
      15+util.Random.nextInt(5)), initialTemperature)
      val bestRun = numRemainedrs(best._1, best._2)
      if (bestRun <= 2) {
        println("Final solution", bestRun)
        println("Tour: " + best._1.sorted + best._2.sorted)
        println(remainders(best._1,best._2))
      }
    }
  }

  main(Array())

}