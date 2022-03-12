object ponder {
  // L1*L2<=600
  // Find a solution for N=11
  def areaOfTriangleDoubled(p12: Set[(Int, Int)], p3: (Int, Int)): Int =
    {
      val p12L = p12.toList
      val xA = p12L(0)._1
      val yA = p12L(0)._2

      val xB = p12L(1)._1
      val yB = p12L(1)._2

      val xC = p3._1
      val yC = p3._2
      scala.math.abs((xA - xC) * (yB - yA) - (xA - xB) * (yC - yA))
    }

  def fancyPrint(setOfPoints: Set[(Int, Int)], L1: Int) =
    {
      val L2 = scala.math.floor(600 / L1).toInt
      /*
  println("There are ", setOfPoints.size, " points")
  setOfPoints.foreach(println)
  */
      /*
  val allGridPoints = for{
   a <- 0 to L1
   b <- 0 to L2
  } yield (a,b)
   val areasAlready: List[((Int,Int),(Int,Int),(Int,Int),Int)] = setOfPoints.subsets(3).map{ tripletOfPoints =>
    val tripletOfPointsList = tripletOfPoints.toList
    ((tripletOfPointsList(0),tripletOfPointsList(1), tripletOfPointsList(2),
    areaOfTriangleDoubled(Set(tripletOfPointsList(0),tripletOfPointsList(1)), tripletOfPointsList(2)))) }.toList.sortBy(_._4)

  println("There are ", areasAlready.size, " triangles")
  println(setOfPoints.size, " choose 3 = ", setOfPoints.size * (setOfPoints.size - 1) * (setOfPoints.size - 2) /6)
  areasAlready.foreach(println)
  */
      println("Lattice ", L1, " by ", L2)
      (0 to L2).reverse.map { b =>
        (0 to L1).map(a => if (setOfPoints contains ((a, b))) 'x' else '.').mkString
      }.foreach(println)
    }

 def gcd(a: Int, b: Int): Int = b match {
  case 0 => a
  case _ => gcd(b, a%b)
 }
 
  def findSolution(L1: Int, allGridPoints: Set[(Int,Int)], setOfPoints: Set[(Int, Int)] = Set(), areasAlready: Set[Int] = Set() ): Boolean = setOfPoints.size match {
    case 11 => {
      fancyPrint(setOfPoints, L1)
      true
    }
    case _ => {
      /*if (setOfPoints.size < 8) println("Setsofpoints size", setOfPoints.size, setOfPoints)*/
      if (util.Random.nextInt(1000) == 0) fancyPrint(setOfPoints, L1)
      val L2 = 4/*scala.math.floor(600 / L1).toInt*/
      // To speed things up, we'll avoid duplicate lattices by ensuring that "this point" must be "greater" than all the points in setsOfPoint
      val greatestPointRowMajorIndex = if (setOfPoints.isEmpty)
        -1
      else
        setOfPoints.map(p => p._1 + p._2 * (L1+1)).max
      
      //fancyPrint(allGridPoints.toSet, L1)
      /*allGridPoints.exists(point => {
        val areas = setOfPoints.subsets(2).map(pairOfPoints => areaOfTriangleDoubled(pairOfPoints, point)).toList
        !areas.contains(0) &&
          areas.distinct.size == setOfPoints.size * (setOfPoints.size - 1) / 2 &&
          !areas.exists(areasAlready.contains) &&
          findSolution(L1, allGridPoints, setOfPoints + point, areasAlready ++ areas)*/
 // Looks like we can do ((setOfPoints choose 2) choose 2) to get all pairs of lines, then we can run Euclid's Extended Algorithm to filter out new points.
 // But we don't need all pairs of lines, just every line in (SetofPoints choose 2) and every line (point - each element of setOfPoints). That is n*(n-1)/2 * n, which is like 500 at most.

      val newGridPoints = allGridPoints.filter(point => {
        val areas = setOfPoints.subsets(2).map(pairOfPoints => areaOfTriangleDoubled(pairOfPoints, point)).toList
        !setOfPoints.contains(point) &&  !areas.contains(0) &&
          areas.distinct.size == setOfPoints.size * (setOfPoints.size - 1) / 2 &&
          !areas.exists(areasAlready.contains) })
//   println(setOfPoints.size)
//   println(newGridPoints.size)
   //newGridPoints.foreach(println)
    // So we want to sort by something about the relative primeness of the x and y distances from this point to each element of setOfPoints.
    // Like a good point we want to explore first would have distances like (13,7), (5,11),... , and no entries like (0,5), (8,4). Even 1 entry with a 0 in
    // it we'd like to avoid. I suppose we could just divide by greatest common divisor,
    
   
      newGridPoints.toSeq.sortBy{ point =>
          val areas = setOfPoints.subsets(2).map(pairOfPoints => areaOfTriangleDoubled(pairOfPoints, point)).toList
          if (areas.isEmpty) 0 else
          - areas.min
/*  	if (setOfPoints.size == 0)
  	 0
  	 else {
       val primeScores = setOfPoints.map{ p=>
        val gcdresult = gcd(scala.math.abs(p._1 - point._1), scala.math.abs(p._2 - point._2))
        if (gcdresult == 0)
         0
        else
         scala.math.min(scala.math.abs(p._1 - point._1)/ gcdresult, scala.math.abs(p._2 - point._2) / gcdresult)
       }
       -primeScores.min
       //-primeScores.product
       
       }*/
      }.take(6).exists { point =>
        val areas = setOfPoints.subsets(2).map(pairOfPoints => areaOfTriangleDoubled(pairOfPoints, point)).toList
        findSolution(L1, newGridPoints, setOfPoints + point, areasAlready ++ areas)}
      
    }
  }

  def findSolutionPointsOnEdges(L1: Int) /*: Boolean*/ = {
    val L2 = 4/*scala.math.floor(600 / L1).toInt*/
    val solutionSpace = for {
      //  garbage - can't have more than 1 row or column with 2 points on it.
      setOfPointsOnLeft <- (1 to L2 - 1).toSet.subsets(2).map(x => x.map(y => (0, y)))
      setOfPointsOnRight <- (1 to L2 - 1).toSet.subsets(2).map(x => x.map(y => (L1 - 1, y)))
      setOfPointsOnBottom <- (1 to L1 - 1).toSet.subsets(2).map(x => x.map(y => (y, 0)))
      setOfPointsOnTop <- (1 to L1 - 1).toSet.subsets(2).map(x => x.map(y => (y, L2 - 1)))
      setOfPointsInMiddle <- (for {
        b <- 1 to L2 - 1
        a <- 1 to L1 - 1
        /*    if (a+b*L1 > greatestPointRowMajorIndex &&
     ( setOfPoints.size < 3 && a <= L1/2 && b <= L2/2 || setOfPoints.size >= 3 ))*/
      } yield (a, b)).toSet.subsets(3)
    } yield setOfPointsOnLeft ++ setOfPointsOnRight ++ setOfPointsOnBottom ++ setOfPointsOnTop ++ setOfPointsInMiddle

    val sol = solutionSpace.find(s => {
      if (util.Random.nextInt(1000) == 0) fancyPrint(s, L1)
      score(s.toList, L1, L2) == 0
    })
    sol match {
      case Some(s) => fancyPrint(s, L1)
      case _       => println("No solution for ", L1)
    }
  }

  def createNeighbour(s: List[(Int, Int)], L1: Int, L2: Int): List[(Int, Int)] = {
    val ns = List(
      (-1, -1),
      (-1, 0),
      (-1, 1),
      (0, -1),
      (0, 1),
      (1, -1),
      (1, 0),
      (1, 1))
    val r = scala.util.Random.nextInt(s.size)
    val nsr = ns(scala.util.Random.nextInt(ns.size))
    s.updated(r, ((s(r)._1 + nsr._1 + L1) % L1, (s(r)._2 + nsr._2 + L2) % L2))
  }

  def allNeighbours(s: List[(Int, Int)], L1: Int, L2: Int) = {
    val ns = List(
      (-1, -1),
      (-1, 0),
      (-1, 1),
      (0, -1),
      (0, 1),
      (1, -1),
      (1, 0),
      (1, 1))
    // val r = scala.util.Random.nextInt(s.size)
    // val nsr = ns( scala.util.Random.nextInt(ns.size))
    for {
      r <- 0 until s.size
      nsp <- ns
    } yield s.updated(r, ((s(r)._1 + nsp._1 + L1) % L1, (s(r)._2 + nsp._2 + L2) % L2))

  }

  /* val L1=24
 val L2=25
 */
  // val startingList = List.fill(11)((scala.util.Random.nextInt(L1), scala.util.Random.nextInt(L2)))
  //createNeighbour(startingList, L1, L2)
  val (initialTemperature, finalTemperature, coolingRate) = (200.0, 0.001, 0.0005)

  def score(s: List[(Int, Int)], L1: Int, L2: Int): Int = {
    val triangleList = s.toSet.subsets(3).toList
    val areas = triangleList.map { tripletOfPoints =>
      val pointsList = tripletOfPoints.toList
      areaOfTriangleDoubled(Set(pointsList(0), pointsList(1)), pointsList(2))
    }
    /*println( "zeros ", areas.count(_==0)," distincts ", areas.distinct.size)
  println(areas.sorted)
  */
    areas.count(_ == 0) + 165 - areas.distinct.size // 165 == 11 choose 3
  }

  def simulatedAnnealing(best: List[(Int, Int)], temp: Double, L1: Int, L2: Int): List[(Int, Int)] = {
    if (temp > finalTemperature) {
      val currentEnergy = score(best, L1, L2)
      val neighbour = createNeighbour(best, L1, L2)
      //   println("curr neigh", best, neighbour)
      val neighbourEnergy = score(neighbour, L1, L2)
      println("curr neigh temp", currentEnergy, neighbourEnergy, temp)
      if (neighbourEnergy == 0) {
        println(neighbour)
        neighbour
      } else { // Decide if we should accept the neighbour
        val accept = (math.exp((currentEnergy - neighbourEnergy) / temp) > math.random)
        simulatedAnnealing(if (accept) {
          // println("\nScore: " + neighbourEnergy, " Temperature: "+ temp)
          neighbour
        } else best, (1 - coolingRate) * temp, L1, L2)
      }
    } else best
  }

  def geneticAlgorithm(pop: IndexedSeq[List[(Int, Int)]], generation: Int, L1: Int, L2: Int, populationSize: Int): IndexedSeq[List[(Int, Int)]] = {

    // Evaluation and selection
    // Take the best half
    val bestOfPop = util.Random.shuffle(pop.sortBy { x => score(x, L1, L2) }.take(populationSize / 2))

    // Crossover
    // Each couple has 2 children
    val newPopulation = ((bestOfPop take populationSize / 4) zip (bestOfPop drop populationSize / 4)) flatMap {
      case (i1, i2) =>
        (1 to 2) map { _ => (i1 zip i2) map { case (el1, el2) => if (util.Random.nextBoolean()) el1 else el2 }
        }
    } // flatMap (x=> IndexedSeq(x,x))

    // Mutation
    val mutatedPopulation = newPopulation map { pop =>
      pop map { point =>
        if (util.Random.nextDouble() < .01)
          if (util.Random.nextBoolean())
            ((point._1 + 1 + L1) % L1, point._2)
          else
            (point._1, (point._2 + 1 + L2) % L2)
        else
          point
      }
    }

    if (generation < 100)
      geneticAlgorithm(mutatedPopulation, generation + 1, L1, L2, populationSize)
    else
      newPopulation
  }

  def greedy(s: List[(Int, Int)], L1: Int, L2: Int): List[(Int, Int)] = {
    println("Greedy " + score(s, L1, L2))
    val betterNeighbours = allNeighbours(s, L1, L2).filter { x => score(x, L1, L2) < score(s, L1, L2) }
    if (betterNeighbours.isEmpty)
      s
    else
      greedy(betterNeighbours.minBy { x => score(x, L1, L2) }, L1, L2)
  }

  def rankPoints(L1: Int) = {
    val L2 = scala.math.floor(600 / L1).toInt
    val allGridPoints = for {
      b <- 0 to L2
      a <- 0 to L1
    } yield (a, b)

    val areas = allGridPoints.toSet.subsets(2).map { pairOfPoints => areaOfTriangleDoubled(pairOfPoints, (0, 0)) }.toList.groupBy { identity }.mapValues(_.size)

    //  areas.toSeq.sortBy(- _._2).foreach(println)
    println(areas.size)
    false

    /*   allGridPoints.exists( point => {
     val areas = setOfPoints.subsets(2).map( pairOfPoints => areaOfTriangleDoubled(pairOfPoints, point) ).toList
     !areas.contains(0) &&
     areas.distinct.size == setOfPoints.size * (setOfPoints.size - 1)/2 &&
     !areas.exists(areasAlready.contains) &&
     findSolution(L1, setOfPoints + point, areasAlready ++ areas) } )
*/
  }

  def main(args: Array[String]) = {
    if (args.contains("Sim")) {
      println("Starting Simulated Annealing")
      //if (false)
      // Simulated Annealing
      Stream.from(1) find { x =>
        val L1 = 4 /*scala.util.Random.nextInt(15) + 10*/
        val L2 = 4 /*scala.math.floor(600 / L1).toInt*/
        val best = simulatedAnnealing(List.fill(4/*11*/)((scala.util.Random.nextInt(L1), scala.util.Random.nextInt(L2))), initialTemperature, L1, L2)
        val result = score(best, L1, L2)
        println(L1, L2, "Last results were " + result + ". Running again. " + x)
        result <= 0
      }
    } else if (args.contains("Gen")) {
      val populationSize = 40 // must be a multiple of 4
      println("Starting Genetic Algorithm")
      Stream.from(1) find { x =>
        //Initialization
        val L1 = 4 /*scala.util.Random.nextInt(15) + 10*/
        val L2 = 4 /*scala.math.floor(600 / L1).toInt*/
        val population = (1 to populationSize) map (_ => List.fill(4/*11*/)((scala.util.Random.nextInt(L1), scala.util.Random.nextInt(L2))))
        val result = geneticAlgorithm(population, 0, L1, L2, populationSize).map(x => score(x, L1, L2)).min
        println(L1, L2, "Last results of genetic algorithm were " + result + ". Running again. " + x)
        result <= 0
      }
    } else if (args.contains("Greedy")) {
      println("Starting Greedy")
      Stream.from(1) find { x =>
        //Initialization
        val L1 = 4 /*scala.util.Random.nextInt(15) + 10*/
        val L2 = 4 /*scala.math.floor(600 / L1).toInt*/

        val bestGreedy = greedy(List.fill(4/*11*/)((scala.util.Random.nextInt(L1), scala.util.Random.nextInt(L2))), L1, L2)
        val result = score(bestGreedy, L1, L2)
        println(L1, L2, "Last results of greedy algorithm were " + bestGreedy + " with score " + result + ". Running again. " + x)
        result <= 0
      }
    } else if (args.contains("edges")) {
      findSolutionPointsOnEdges(23)
    } else
{
     val oleg = List( (20,0), (0,1), (3,13), (7,18), (6,2), (23,22), (4,23), (24,9), (19,24), (12,2), (23,7))
println(score(oleg, 24,24))
val alper = List((11,0),(66,1),(0,2),(53,3),(5,4),(65,5),(63,6),(1,7),(18,8),(8,9),(66,9))
println(score(alper,66,9))
val Hermann = List((0,12),(1,6),(2,1),(4,15),(6,2),(17,3),(23,16),(27,16),(32,0),(32,14),(33,5))
println(score(Hermann,33,16))
 fancyPrint(oleg.toSet, 24)
 fancyPrint(alper.toSet, 66)
 fancyPrint(Hermann.toSet, 33)
 println("Done printing")
 println(findSolution(24, Set(), oleg.toSet, Set()))
 println(findSolution(66, Set(), alper.toSet, Set()))
 println(findSolution(33, Set(), Hermann.toSet, Set()))
 
 
      // since there can be at most 2 points per row, we need at least 6 rows to accomodate 11 points.
      // We can do even better. Suppose there are 2 rows with 2 points each. Those 4 points will make 2 triangles with the same area!
      // So there can exist at most 1 row with 2 points on it. So that means L1 is at least 10.
      (10 to 24).reverse.find { L1 =>
        println("L1 = ", L1)
        rankPoints(L1)
      }

      (4 to 4 /*10 to 24*/).reverse.find{L1 =>
  println("L1 = ", L1)
       val L2 = 4 /*scala.math.floor(600 / L1).toInt*/
  val allGridPoints = (for {
      b <- 0 to L2
      a <- 0 to L1
    } yield (a, b)).toSet
  findSolution(L1, allGridPoints)}
 }
  }
}