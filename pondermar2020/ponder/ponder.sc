import java.io._
import scala.io.Source
object ponder {
  def main(args: Array[String]) = {

  val file = new File("svg2.svg")
  val bw = new BufferedWriter(new FileWriter(file))

    def mapOffset(point: (Int, Int)) =
      {
        Set(
          (-2, -2), (-1, -1), (1, 1),  (2, 2), // up and to the right
          (-2, 0),  (-1, 0),  (1, 0),  (2, 0), // horizontal
          (-2, 2),  (-1, 1),  (1, -1), (2, -2), // down and to the right
          (0, -2),  (0, -1),  (0, 1),  (0, 2) // vertical
        )
        .map(o => (o._1 + point._1, o._2 + point._2))
      }
    def mapOffsetKnight(point: (Int, Int)) =
      {
        Set((-2, -1), (-2, 1), (2, -1), (2, 1),
            (-1, -2), (-1, 2), (1, -2), (1, 2))
        .map(o => (o._1 + point._1, o._2 + point._2))
      }

    def notTooClose(holesSoFar: Set[(Int, Int)], point: (Int, Int)): Boolean = {
    // We are going to allow (1,2) and (2,1)
    // (1,9) and (2,10)
    // (9,10) and (10,9)
    // (9,1) and (10,2)
    val cornerPoints = Set (
     (1,2),  (2,1) /*,
     (1,9),  (2,10),
     (9,10), (10,9),
     (9,1),  (10,2)*/)
    
    if ( cornerPoints contains point )
      mapOffset(point) intersect(holesSoFar -- cornerPoints) isEmpty
     else
      mapOffset(point) intersect(holesSoFar) isEmpty
      //!holesSoFar.exists(mapOffset(point).contains)
    }


    def printGrid(grid: Set[(Int, Int)]) =
      (15 to 1 by -1).map { y =>
        println((1 to 15).map { x => if (grid.contains((x, y))) "o" else "." }.mkString)
      }

    val grid10x10 = (for {
      x <- 1 to 10
      y <- 1 to 10
    } yield (x, y)).toSet
    val grid5x5 = (for {
      x <- 1 to 5
      y <- 1 to 5
    } yield (x, y)).toSet
    val grid5x10 = (for {
      x <- 1 to 5
      y <- 1 to 10
    } yield (x, y)).toSet
    val grid10x5 = (for {
      x <- 1 to 10
      y <- 1 to 5
    } yield (x, y)).toSet

    val grid3Quarters = (for {
      x <- 1 to 5
      y <- 1 to 10
    } yield (x, y)).toSet ++
      (for {
        x <- 6 to 10
        y <- 1 to 5
      } yield (x, y)).toSet

  val grid15x15FullGrid = (for {
   x <- 1 to 15
   y <- 1 to 15}
   yield (x,y)).toSet
   
   
   
  def hasCliqueOfSize4(holes: Set[(Int, Int)]) =
   (grid15x15FullGrid -- holes).exists { point =>
    val n1s = neighbours(holes, point)
    //println("point" + point)
    //println("neighbours of point " + n1s)
    // Is there a set of 3 neighbours who are all neighbours with each other
    n1s.subsets(3).exists { n2 =>
     //println("n2 " + n2)
     n2.subsets(2).forall { setof2 =>
      // println("setof2 " + setof2)
      val listOf2 = setof2.toList
      val a = listOf2(0)
      val b = listOf2(1)
      // println("a b " + a + " " + b)
      // println("neighbours a " + neighbours(holes, a))
      neighbours(holes, a).contains(b)
     }
    }
   }



    def printGridPath(sol: Map[(Int, Int), Int]) =
      (15 to 1 by -1).map { y =>
        println((1 to 15).map { x =>
          sol.get((x, y)) match {
            case Some(c) => c //.toChar
            case None    => "."
          }
        }.mkString)
      }

    def printGridPlacedAndHoles(placed: Map[(Int, Int), Int], holes: Set[(Int, Int)]) =
      (15 to 1 by -1).map { y =>
        println((1 to 15).map { x =>
          placed.get((x, y)) match {
            case Some(c) => c //.toChar
            case None    => if (holes.contains((x, y))) "o" else "."
          }
        }.mkString)
      }
      
    // So we could represent the candidates within the Set(1,2,3) as a binary number from 1 to 7. It would be 0 if it is empty.
    def printGridRemaining(sol: Map[(Int, Int), Set[Int]]) =
      (15 to 1 by -1).map { y =>
        println((1 to 15).map { x =>
          sol.get((x, y)) match {
            case Some(c) => c.map(cand => math.pow(2, (cand - 1))).sum.toInt //.toChar
            case None    => "."
          }
        }.mkString)
      }

    def updateRemaining(newPlacementPosition: (Int, Int), newValue: Int, remaining: Map[(Int, Int), Set[Int]], holes: Set[(Int, Int)], placed: Map[(Int, Int), Int]): Map[(Int, Int), Set[Int]] =
      {
        // Consider placed with newValue the same as holes.
        val holesToConsider = mapOffset(newPlacementPosition) intersect holes ++
          (mapOffset(newPlacementPosition) intersect placed.keys.toSet filter (placed(_) == newValue))
        val remainingPositionsToUpdate = holesToConsider.flatMap { hole =>
          val dx = hole._1 - newPlacementPosition._1
          val dy = hole._2 - newPlacementPosition._2
          val d = scala.math.max(dx.abs, dy.abs)

          // When it is 2 away from a hole, we only have to cancel the one in between.
          // hole  x  new
          // However, when it is only 1 away from a hole, right next to it, then we have to cancel
          // both on the far side of the hole, and on the other side of the new.
          // xFar  hole  new  xOther

          d match {
            case 2 => List((newPlacementPosition._1 + dx / 2, newPlacementPosition._2 + dy / 2))
            case 1 => List(
              (newPlacementPosition._1 + dx * 2, newPlacementPosition._2 + dy * 2),
              (newPlacementPosition._1 - dx, newPlacementPosition._2 - dy))

            case _ => {
              println("Should not have got here!!!")
              List()
            }
          }
        }
        // OK.... So we filter the neighbours that are == newValue.
        // We calculate their d
        // For d==2, we see if there's an entry of remaining for the d=1 and remove that candidate.
        // for d==1 .... wait a sec! We are doing the exact same thing.

        // Now they should have have candidate removed from their Set values
        // We'll use flatMap to get rid of the Option, which will be for positions not in Remaining or off the grid
        val remainingPositionsAndValuesToUpdate = remainingPositionsToUpdate.flatMap { remainingPosition =>
          remaining.get(remainingPosition) match {
            case Some(setOfCandidates) => Some(remainingPosition, setOfCandidates - newValue)
            // We have to use the None case because we could be trying to update a cell that's already been placed, or a cell off the grid.
            case None                  => None
          }
        }
        
        // We add (++) them back in to replace the old values, even though the new values will be smaller.
        remaining ++ remainingPositionsAndValuesToUpdate - newPlacementPosition
      }

    def neighbours(holes: Set[(Int, Int)], point: (Int, Int)): Set[(Int,Int)] = {
      val holesToConsider = mapOffset(point) intersect holes
      holesToConsider.flatMap { hole =>
        val dx = hole._1 - point._1
        val dy = hole._2 - point._2
        val d = scala.math.max(dx.abs, dy.abs)

        // When it is 2 away from a hole, we only have to cancel the one in between.
        // hole  x  new
        // However, when it is only 1 away from a hole, right next to it, then we have to cancel
        // both on the far side of the hole, and on the other side of the new.
        // xFar  hole  new  xOther

        d match {
          case 2 => Set((point._1 + dx / 2, point._2 + dy / 2))
          case 1 => Set(
            (point._1 + dx * 2, point._2 + dy * 2),
            (point._1 - dx, point._2 - dy))

          case _ => {
            println("Should not have got here!!!")
            List()
          }
        }
      }
    }

  def printSVG(remaining: Map[(Int, Int), Set[Int]], placed: Map[(Int, Int), Int], holes: Set[(Int, Int)], hPos: Int, lowestCandidateCount: (Int,Int) ) = {
   val top = (80 - remaining.size)*200
   val vLines = for ( i <- (0 to 100 by 10)) yield "<line x1=\"" + (500*hPos+i) + "\" y1=\"" + top  + "\" x2=\"" + (500*hPos+i) + "\" y2=\"" + (top +100) + "\" style=\"stroke:rgb(0,0,0);stroke-width:1\" />\n"
   val hLines = for ( j <- (0 to 100 by 10)) yield "<line x1=\"" + 500*hPos + "\" y1=\"" + (top+j)  + "\" x2=\"" + (500*hPos+100) + "\" y2=\"" + (top + j ) + "\" style=\"stroke:rgb(0,0,0);stroke-width:1\" />\n"
   
   val remainingRectangles = remaining.flatMap{ case(cell, setOfCands) => setOfCands.map{cand => cand match {
    case 1 => "<rect x=\"" + (500*hPos + cell._1 * 10 -10) + "\" y=\"" + (top - cell._2 * 10 + 100) + "\" width=\"3\" height=\"3\" style=\"fill:rgb(0,0,255);stroke-width:0;stroke:rgb(0,0,0)\" />\n"   // <rect width="300" height="100" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" />
    case 2 => "<rect x=\"" + (500*hPos + cell._1 * 10 -7 ) + "\" y=\"" + (top - cell._2 * 10 + 100) + "\" width=\"3\" height=\"3\" style=\"fill:rgb(0,255,0);stroke-width:0;stroke:rgb(0,0,0)\" />\n"   // <rect width="300" height="100" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" />
    case 3 => "<rect x=\"" + (500*hPos + cell._1 * 10 -4 ) + "\" y=\"" + (top - cell._2 * 10 + 100) + "\" width=\"3\" height=\"3\" style=\"fill:rgb(255,0,0);stroke-width:0;stroke:rgb(0,0,0)\" />\n"   // <rect width="300" height="100" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" />
   } } }


  // Can make this a rect based on colour...
  val placedStrings = placed.map{ case(cell, pInt) => "<text x=\"" + (500*hPos + cell._1 * 10 -10) + "\" y=\"" + (top - cell._2 *10 +110) + "\" fill=\"black\">" + pInt.toString + "</text>\n"  }
  
	val holesStrings = holes.map{ case(x, y) =>  "<circle cx=\"" + (500*hPos + x * 10 -5) + "\" cy=\"" + (top -y * 10 + 105) + "\" r=\"4\" stroke=\"red\" stroke-width=\"0\" fill=\"red\" /> \n" }
	
  val lowestCandString = "<rect x=\"" + (500*hPos + lowestCandidateCount._1 * 10 - 10 ) + "\" y=\"" + (top - lowestCandidateCount._2 * 10 + 100) + "\" width=\"10\" height=\"10\" style=\"fill:rgb(255,255,0);stroke-width:1;stroke:rgb(0,0,0)\" />\n"

	bw.write(lowestCandString + "\n")
  holesStrings.foreach(bw.write)
	remainingRectangles.foreach(bw.write)
  placedStrings.foreach(bw.write)
	vLines.foreach(bw.write)
	hLines.foreach(bw.write)
  }


  
  val history = scala.collection.mutable.ListBuffer[ /*remaining*/(Map[(Int, Int), Set[Int]], /*placed */ Map[(Int, Int), Int],
    /*holes*/ Set[(Int, Int)], Int /*hpos*/, /* lowestCandidate*/(Int,Int) ) ] ()
  
  
  
  
  def listAllOneTwoThrees(remaining: Map[(Int, Int), Set[Int]], placed: Map[(Int, Int), Int],
   holes: Set[(Int, Int)], hPos: Int = 0, countOfPlace: Map[Int, Int] = Map( 1->0, 2->0, 3->0)): Iterator[Map[(Int, Int), Int]] = remaining.size match {
   case 0 => if (countOfPlace.values.max < 63 ) Iterator(placed) else Iterator() // Some(Map()) // Some(placed) // Maybe Some( Map() ) and do the recursive path building thing...
   case _ => {
   	if ((placed.size == 0 && hasCliqueOfSize4(holes)) || countOfPlace.values.max == 63)
   	 Iterator()
 	  else
    {
     val lowestCandidateCount = remaining.filter(cell =>
       cell._2.size == remaining.minBy { _._2.size }._2.size).maxBy(cell => neighbours(holes, cell._1).size)
     for {
      candidate <- if (placed.size <= 1)
            lowestCandidateCount._2.take(1).toIterator
           else
            lowestCandidateCount._2.toIterator
      rest <- listAllOneTwoThrees(updateRemaining(lowestCandidateCount._1, candidate, remaining, holes, placed),
                   placed.updated(lowestCandidateCount._1, candidate), holes, hPos, countOfPlace.updated(candidate, countOfPlace(candidate) + 1))
     } yield rest
   }
    /*
    (if (placed.size <= 1)
           lowestCandidateCount._2.take(1).toIterator
          else
           lowestCandidateCount._2.toIterator)
           .flatMap(candidate => listAllOneTwoThrees(updateRemaining(lowestCandidateCount._1, candidate, remaining, holes, placed),
                  placed.updated(lowestCandidateCount._1, candidate), holes, hPos)
    */
    
    
   }
  }
  


    var findCount = 0
    def findOneTwoThrees(remaining: Map[(Int, Int), Set[Int]], placed: Map[(Int, Int), Int], holes: Set[(Int, Int)], hPos: Int = 0): Option[Map[(Int, Int), Int]] = remaining.size match {
      case 0 => Some(placed) // Some(Map()) // Some(placed) // Maybe Some( Map() ) and do the recursive path building thing...
      case _ => {
       if (placed.size == 0 && hasCliqueOfSize4(holes))
   	    None
   	   else
   	   {
        val lowestCandidateCount = remaining.filter(cell => cell._2.size == remaining.minBy { _._2.size }._2.size).maxBy(cell => neighbours(holes, cell._1).size)
        if (findCount % 100000000 == 0) {
          /*println("findCount " + findCount)
          println("remaining size " + remaining.size)
          println("placed size " + placed.size)
          println("holes size " + holes.size)
          println("Remaining...")
          printGridRemaining(remaining)
          println("Placed")
          printGridPath(placed)
          println("Holes")
          printGrid(holes)
          println("Placed And Holes")
          printGridPlacedAndHoles(placed, holes)
          println("Lowest Candidate")
          println(lowestCandidateCount)
          println("hPos")
          println(hPos)*/
          history.append( (remaining, placed, holes, hPos, lowestCandidateCount._1 ))
        }
        findCount = findCount + 1

        def findLoop(candidates: Set[Int], hPos2: Int): Option[Map[(Int, Int), Int]] = candidates.headOption match {
          case None => None
          case Some(candidate) => findOneTwoThrees(
                    updateRemaining(lowestCandidateCount._1, candidate, remaining, holes, placed),
                    placed.updated(lowestCandidateCount._1, candidate), holes, hPos2)
                    match {
                   	 case Some(solution) => Some(solution) // Some(solution.updated(lowestCandidateCount._1, candidate))
                     case None           => findLoop(candidates - candidate, hPos2 + 1)
                   }
        }
        // Without loss of generality, the first two placements can be "1" and "2" respectively, no need to interate over the other candidates.
        if (placed.size <= 1)
          findLoop(Set(lowestCandidateCount._2.head), 0)
        else
          findLoop(lowestCandidateCount._2, hPos)
      }
    }
    }

    val finalSolution = Set((7, 5), (1, 5), (7, 9), (6, 7), (3, 9), (6, 1), (5, 10), (10, 10), (10, 1), (1, 1), (1, 10), (4, 6), (9, 8), (8, 2), (5, 4), (10, 6), (2, 7), (4, 2), (2, 3), (9, 4))
    println(findOneTwoThrees((grid10x10 -- finalSolution).map(x => x -> Set(1, 2, 3)).toMap, Map(), finalSolution))

    val holesTest = Set((1, 1), (5, 1), (3, 2), (4, 4), (8, 4), (2, 5), (6, 5), (9, 6), (1, 7), (7, 7), (10, 8), (2, 9), (8, 9), (6, 10))
    println("StartTest " + findCount)
    val holesTestResult = findOneTwoThrees((grid5x5 -- holesTest).map(x => x -> Set(1, 2, 3)).toMap, Map(), holesTest)
    println("End Test " + findCount)
    println(holesTestResult)
    // Found nothing wrong with subsets(3) as expected
    /*val xxx = holesTest.subsets().map( hT => (hT, findOneTwoThrees((grid -- hT).map( x => x -> Set(1,2,3)).toMap , Map(), hT)))

  xxx.foreach{ x => x match {
  case (xres, Some(holesTestResult)) => {
   /*println("found s")
   printGridPlacedAndHoles(holesTestResult, xres)
   printGrid(xres)
   printGridPath(holesTestResult)
   */

   }
  case (xTest, None) => {
  println("No Solution to following subset with size " + xTest.size)
  printGrid(xTest)
  }
  }}
  */


    println("clique test start")
    printGrid(holesTest)
    println(hasCliqueOfSize4(holesTest))
    println("Clique test end")

    var databaseOfErrors = scala.collection.mutable.Set[Set[(Int, Int)]]()

    //val lines = Source.fromFile("databaseOfErrors.txt").getLines.toSet

    //databaseOfErrors += lines.map(_.toSet)

    def isItAKnownError(holesSoFar: Set[(Int, Int)]): Boolean =
      !databaseOfErrors.exists(err => err.subsetOf(holesSoFar)) match {
        case true => {
          println("Start of known error1 with")
          // Add to the database of errors
   printGrid(holesSoFar)
  val biggestLoser = holesSoFar.subsets(holesSoFar.size -1).find ( sub => findOneTwoThrees((grid10x10 -- sub).map(x => x -> Set(1, 2, 3)).toMap, Map(), sub) match {
  case Some(a) => false
   case _ => true}) match {
   case Some(foundOneErrorSubset) => isItAKnownError(foundOneErrorSubset)
   case _ => {
   println("Found the biggest")
   printGrid(holesSoFar)} // All the subsets are fine!
   }

          // Now, we can search for this essential error in two ways
          // 1. Subsets starting from smallest, finding the first
          // 2. Subsets of size (n-1), recursively, finding the last.
          // So 1 will involve about half of 2^size successful finds before we get the first error. "Finds" are quick though.
          // 2 will only involve a quadratic number of fails, but the fails are probably exponetial
          //   is approximately the central binomial coeffiencents, so just under exponential.
          val essentialError = holesSoFar.subsets.map(sub => (sub, findOneTwoThrees((grid10x10 -- sub).map(x => x -> Set(1, 2, 3)).toMap, Map(), sub))).find { sub =>
            sub match {
              case (xres, None) => {
                val bw = new BufferedWriter(new FileWriter("databaseOfErrors.txt"))
                xres.foreach(elem => bw.write(elem.toString))
                bw.write("\n")
                bw.flush
                bw.close
                databaseOfErrors += xres
                println("finish of known error1")

                true
              }
              case _ => false // should never get here
            }
          }
          true
        }

        case false => false
      }
   //isItAKnownError(finalSolution)
   // returns
   /*
    Found the biggest
....o.....
..o...o...
........o.
.o...o....
...o......
o.....o...
....o.....
.o........
...o......
..........
   */

    var createHolesCount = 0

    def createHoles(holesSoFar: Set[(Int, Int)], candidates: Set[(Int, Int)], knightMoves: Set[(Int, Int)], n: Int = 20): Iterator[Set[(Int, Int)]] =
      if (databaseOfErrors.contains(holesSoFar))
        Iterator()
      else {
        databaseOfErrors += holesSoFar

        holesSoFar.size match {
          case s if s == n => { /// ******** FFFIXXXXX MEEEE!!! return to 20
            //println("Solution ...")
            // printGrid(holesSoFar)
            Iterator(holesSoFar)
          }
          case s if s < n - 10 => {
            val goodCandidates = candidates.filter(notTooClose(holesSoFar, _))
            if (createHolesCount % 100000000 == 0) {
              println("createholes count " + createHolesCount)
              println("n " + n)
              println("DatabaseofErrors size " + databaseOfErrors.size)
              println("HolesSo far ", holesSoFar.size)
              printGrid(holesSoFar)
              println("candidates ", candidates.size)
              printGrid(candidates)
              println("knightmoves " + knightMoves.size)
              printGrid(knightMoves)
              println("goodcandidates ", goodCandidates.size)
              printGrid(goodCandidates)
            }
            createHolesCount = createHolesCount + 1

            if (candidates.size + holesSoFar.size >= n /* && !databaseOfErrors.contains(holesSoFar)*/ && !hasCliqueOfSize4(holesSoFar)) //&& isItAKnownError(holesSoFar))
              /*findOneTwoThrees((grid -- holesSoFar).map( x => x -> Set(1,2,3)).toMap , Map(), holesSoFar) match {
     case Some(a) => { */
              for {
                goodCandidate <- knightMoves.toIterator
                val nextCandidateSet = candidates -- mapOffset(goodCandidate) - goodCandidate
                holes <- createHoles(holesSoFar + goodCandidate, nextCandidateSet, (knightMoves ++ mapOffsetKnight(goodCandidate)) intersect nextCandidateSet, n)
              } yield holes
            // }  // case Some(a) => {
            //case _ => { // findOneTwoThree returned None, meaning there's no way to place 1 2 or 3s, even with no further holes!
            //     Iterator()
            //   } // case _ => {
            // } // match (findOneTwoThrees
            else {
              // Let's add it to the database of errors so we don't come back this way.
              //println("found error1 " + databaseOfErrors.size + " candidate size " + candidates.size + " holes size " + holesSoFar.size)
              databaseOfErrors += holesSoFar
              Iterator() // There's no possible way to solve
            }
          } // case s if s < 15 => {
          case _ => {
            if (createHolesCount % 100000000 == 0) {
              println("s> 2 createholes count " + createHolesCount)
              println("n " + n)
              println("DatabaseofErrors size " + databaseOfErrors.size)
              println("HolesSo far ", holesSoFar.size)
              printGrid(holesSoFar)
              println("candidates ", candidates.size)
              printGrid(candidates)
              println("knightmoves " + knightMoves.size)
              printGrid(knightMoves)
            }
            createHolesCount = createHolesCount + 1

            if (candidates.size + holesSoFar.size >= n /*&& !databaseOfErrors.contains(holesSoFar) */ && !hasCliqueOfSize4(holesSoFar)) // && isItAKnownError(holesSoFar))
              for {
                goodCandidate <- candidates.toIterator
                val nextCandidateSet = candidates -- mapOffset(goodCandidate) - goodCandidate
                holes <- createHoles(holesSoFar + goodCandidate, nextCandidateSet, (knightMoves ++ mapOffsetKnight(goodCandidate)) intersect nextCandidateSet, n)
              } yield holes
            else {
              //println("found error2 " + databaseOfErrors.size + " candidate size " + candidates.size + " holes size " + holesSoFar.size)
              databaseOfErrors += holesSoFar
              Iterator()
            }
          }
        }
      }
    // Creates the empty Stream as it should (because KnightMoves is empty).
    createHoles(Set(), Set(), Set()).take(2)
    createHoles(Set(), Set((1, 1)), Set((1, 1)))

    grid10x10.filter(notTooClose(Set(), _))
    grid10x10.filter(notTooClose(Set(), _)).size
    grid10x10.filter(notTooClose(Set((1, 1)), _))
    grid10x10.filter(notTooClose(Set((1, 1)), _)).size

    grid10x10.filter(notTooClose(Set((1, 1), (7, 3)), _)).size
    printGrid(grid10x10)
    printGrid(grid10x10.filter(notTooClose(Set((1, 1), (7, 3)), _)))

    //createHoles( Set(), grid).foreach(printGrid)

    val mostlyThere = Set((2, 1), (4, 2), (6, 3), (8, 4), (10, 5),
      (1, 3), (3, 4), (5, 5), (7, 6), (9, 7),
      (2, 6), (4, 7), (6, 8), (8, 9))

    println("mostly there test for clique")
    printGrid(mostlyThere)
    print(hasCliqueOfSize4(mostlyThere))
    println("end!!")
    val mostlySolution = findOneTwoThrees((grid10x10 -- mostlyThere).map(x => x -> Set(1, 2, 3)).toMap, Map(), mostlyThere)
    print(mostlySolution)
    println("Start mostly there create holes")
    createHoles(mostlyThere, grid10x10 -- mostlyThere.flatMap(mapOffset) -- mostlyThere, grid10x10 -- mostlyThere.flatMap(mapOffset) -- mostlyThere).foreach { sol =>
      println("Sol")
      printGrid(sol)
    }
    println("Finished mostly there create holes")

    ////****** We also need to consider neighbours of newPlacementPosition (non holes  --- in the "placed"), since they could cause an accidental win.

    //  val neighbours = mapOffset(newPlacementPosition) intersect placed.toList filter (x => // Huh, I'm surprised this works intersect of a Set and a Map???

    val tHoles = Set((1, 1), (7, 3)) // grid.filter( notTooClose(Set((1,1),(7,3)), _))
    printGrid(tHoles)
    val t = (grid10x10 -- tHoles).map(x => x -> Set(1, 2, 3)).toMap
    printGridRemaining(updateRemaining((6, 2), 2, t, tHoles, Map()))

    tHoles.size
    printGrid(grid10x10 -- tHoles)
    (grid10x10 -- tHoles).size

    updateRemaining((6, 2), 2, t, tHoles, Map())
    printGridRemaining(updateRemaining((6, 2), 2, t, tHoles, Map()))
    updateRemaining((6, 2), 2, t, tHoles, Map()).size
    updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2))
    printGridRemaining(updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2)))
    updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2)).size

    val effected = updateRemaining((6, 2), 2, t, tHoles, Map()).filter { case (k, v) => v.size == 2 }
    val effected2 = updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2)).filter { case (k, v) => v.size == 2 }

    printGridPath(Map((1, 1) -> 1, (4, 1) -> 2, (7, 3) -> 3))
    createHoles(mostlyThere, grid10x10 -- mostlyThere.flatMap(mapOffset) -- mostlyThere, Set()).take(3).toList.map(holes =>
      findOneTwoThrees((grid10x10 -- holes).map(x => x -> Set(1, 2, 3)).toMap, Map(), holes))

    // See if we can find a solution with no constrainst on holes!
    val basicSolution = findOneTwoThrees(grid5x5.map(x => x -> Set(1, 2, 3)).toMap, Map(), Set())
    printGridPath(basicSolution.get)
    val xeu = basicSolution.get.map(x => x._2).groupBy(identity).mapValues(_.size)

    //createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere).take(200).toList.flatMap(holes => findOneTwoThrees( (grid -- holes).map( x => x -> Set(1,2,3)).toMap , Map(), holes) )
    //val solsTest = createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere)./*toList.*/flatMap(holes => findOneTwoThrees( (grid -- holes).map( x => x -> Set(1,2,3)).toMap , Map(), holes) )
    //solsTest.foreach{ printGridPath}

    //createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere).size

    val grid4x4 = (for {
      x <- 1 to 4
      y <- 1 to 4
    } yield (x, y)).toSet

    /*createHoles( Set(), fourByfour).flatMap(holes => findOneTwoThrees( (fourByfour -- holes).map( x => x -> Set(1,2,3)).toMap , Map(), holes) ).foreach{sol =>
  println("Sol")
  printGridPath(sol)}
 */
    // For whatever reason having the foreach on this line leads to some sort of infinite loop???

    /* val sols1 = createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere, Set()).flatMap(holes => findOneTwoThrees( (grid -- holes).map( x => x -> Set(1,2,3)).toMap , Map(), holes) )

 sols1.foreach{sol =>
// println("Sol")
 printGridPath(sol)}
*/
    //val h1 = createHoles( Set(), grid, Set((1,1),(2,1),(3,1)))//.head

    //val h = createHoles( Set(), grid, Set())//.head

    val streamex = Stream(1, 2, 3)

// We can find all the 5x5s, then combine them to find all the 5x10s, then combine those to get all the 10x10s


  // So grid5.subsets(6).... returns 0 results, as expected, making it necessary that all 4 quadrants (or however else you want to partition
  // the 10x10 grid into equal shapes) all need exactly 5 holes.
  val bottomLeft5x5swith6Holes = (grid5x5.subsets(6).filter( grid5x5 => grid5x5.forall(notTooClose(grid5x5, _)))).toSet
  
/*  OOOOOOHHHH MY GOOODDDDDDD!!!!
  We can gave
  not too close,
  Bottom left
  ....
  ....
  ....
  o...
  .o..
  So not too close!!! Isn't that bad!!!
  Is that the only exception???
  */
  
  
  val bottomLeft5x5s = (grid5x5.subsets(5).filter( grid5x5 => grid5x5.forall(notTooClose(grid5x5, _)))).toSet
  println("bottomLeft5x5 size " + bottomLeft5x5s.size)

  val bottomLeft5x5sfiltered = bottomLeft5x5s.filter(holes => findOneTwoThrees((grid5x5 -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => true
   case _ => false
   })
                                                   
  val bottomLeft5x5sfilteredOut = bottomLeft5x5s.filter(holes => findOneTwoThrees((grid5x5 -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => false
   case _ => true
   })
   
   bottomLeft5x5sfilteredOut.foreach(printGrid(_))
                                                      
   println("bottomLeft5x5filtered size " + bottomLeft5x5sfiltered.size)

  val leftS = for{
  bottomLeft <- bottomLeft5x5sfiltered
  
  // We do 11 - , instead of 5+ so the bottom left corner (which has the corner space exeption gets mapped to the top left corner)
  topLeft <- bottomLeft5x5sfiltered.map(_.map( element => (element._1, 11 - element._2 )))
  if (bottomLeft.forall(notTooClose(topLeft, _)))
  }yield bottomLeft ++ topLeft

  println("leftS size" + leftS.size)

  val left5x10sfiltered = leftS.filter(holes => findOneTwoThrees((grid5x10 -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => true
   case _ => false
   })
   println("left5x10filtered size " + left5x10sfiltered.size)

  val full10 = for {
        leftHalf <- left5x10sfiltered
        rightHalf <- left5x10sfiltered.map(_.map(lefthalfElement => (11- lefthalfElement._1, lefthalfElement._2)))
        if (leftHalf.forall(notTooClose(rightHalf, _)))
      } yield leftHalf ++ rightHalf

  println("full10 size " + full10.size)
   full10.foreach{ x =>
    println("Full10 hole " + hasCliqueOfSize4(x))
    println(x)
    printGrid(x)
   }
 

  history.clear()
  findOneTwoThrees((grid10x10 -- full10.head).map(x=> x -> Set(1,2,3)).toMap, Map(), full10.head, 0)
  history.size
	//history.foreach( x => println(x))
  /*bw.write("<svg height=\"2100\" width=\"500\">\n")
  history.foreach(x => printSVG(x._1, x._2, x._3, x._4, x._5 ))
  bw.write("</svg>\n")
  bw.close()
*/


  val full10sfiltered = full10.filter(holes => findOneTwoThrees((grid10x10 -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => true
   case _ => false
   })
   println("full10filtered size " + full10sfiltered.size)
 
   full10sfiltered.foreach{ x =>
    println("Full10filtered hole " + hasCliqueOfSize4(x))
    println(x)
    printGrid(x)
   }
   
   val sols = full10sfiltered.map{ holes => (holes, findOneTwoThrees((grid10x10 -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes))}
   val xeu2 = sols.map( x => (x, x._2.get.map(x => x._2).groupBy(identity).mapValues(_.size)))
   
   val totalSol = xeu2.filter(possibleSolution => possibleSolution._2.values.max  == 27)
   totalSol.foreach{x =>
   println("Solution")
   printGridPlacedAndHoles(x._1._2.get, x._1._1)}
/*
Solution
3o23o1213o
o21212o321
13123313o2
3o232o3211
212o31123o
o23112o313
1132o323o2
3o21223121
132o31313o
o2131o23o2
*/
                                                  
/*
Bonus '*' for a solution that lets the demon act on the 186th move on a 15x15 board; and '**' for the 185th move.
So 15x15=225
Moves number 186 to 225 will be (225-186+1) = 40 holes.
But 225 isn't divisible by 4.
Factors of 40
2 2 2 5.
Factors of 225
3 3 5 5
gcd is 5.
So we can divide the board up into 5, (3 x 15), each with 8 holes?
Start with seeing if we can put 9 holes in a (3x15)?

Or how about 8x7, 8x7, 8x7, 8x7 and 1?
10, 10, 10, 10, and 0 holes. (worry about the other case where the 1 centre cell has a hole later)
Each 8x7 can be 2 4x7, 5 holes each?
28 choose 5 is less than 100,000.


*/

   val grid3x15 = (for {
      x <- 1 to 3
      y <- 1 to 15
    } yield (x, y)).toSet
 
/*
This is hundreds of millions...
val bottomLeft3x15swith9Holes = (grid3x15.subsets(9).filter( g => g.forall(notTooClose(g, _)))).toSet
 val bottomLeft3x15swith8Holes = (grid3x15.subsets(8).filter( g => g.forall(notTooClose(g, _)))).toSet
  */
  

  /*
___________________
|       |    |    |
|       |    |    |
|       |    |    |
|_______|    |    |
|       |    |    |
|    7x4|    |    |
|       |____|____|
|_______|_|       |
|    |    |       |
|    |    |       |
| 4x7|    |_______|
|bottom   |       |
|left|    |       |
| 1  | 2  |       |
|____|____|_______|
  */
  
  val grid4x7bottomLeft1FullGrid = (for {
   x <- 1 to 4
   y <- 1 to 7}
   yield (x,y)).toSet
  val grid4x7bottomLeft2FullGrid = (for {
   x <- 5 to 8
   y <- 1 to 7}
   yield (x,y)).toSet
  val grid8x7LeftFullGrid = (for {
   x <- 1 to 8
   y <- 1 to 7}
   yield (x,y)).toSet
  val grid7point5x15LeftFullGrid = (for {
   x <- 1 to 8
   y <- 8 to 15}
   yield (x,y)).toSet ++ grid8x7LeftFullGrid
    
   
   // Going to try to do this all in one line
   
   
 val grid4x7bottomLeft1 =  (grid4x7bottomLeft1FullGrid.subsets(5).filter ( g => g.forall(notTooClose(g, _)))).withFilter(holes =>
         findOneTwoThrees((grid4x7bottomLeft1FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
          case Some(x) => true
          case _ => false
         }).toStream
  val grid4x7bottomLeft1Size6 =          (grid4x7bottomLeft1FullGrid.subsets(6).filter ( g => g.forall(notTooClose(g, _)))).withFilter(holes =>
         findOneTwoThrees((grid4x7bottomLeft1FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
          case Some(x) => true
          case _ => false
         }).toStream
         
  val grid4x7bottomLeft2Size6 =          (grid4x7bottomLeft2FullGrid.subsets(6).filter ( g => g.forall(notTooClose(g, _)))).withFilter(holes =>
         findOneTwoThrees((grid4x7bottomLeft2FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
          case Some(x) => true
          case _ => false
         }).toStream
 /*
 grid4x7bottomLeft1.size 426
grid4x7bottomLeft1Shouldbe0.size 32
grid4x7bottomLeft2.size368
grid4x7bottomLeft2Shouldbe0.size 25
grid4x7bottomLeft1filtered.size 426
grid4x7bottomLeft2filtered.size 368
gridBottomLeftCombined.size 9033
gridBottomLeftCombinedFiltered.size 7744
 
 */
 
 
  println("grid4x7bottomLeft1.size " + grid4x7bottomLeft1.size)
  val grid4x7bottomLeft1Shouldbe0 = (grid4x7bottomLeft1FullGrid.subsets(6).filter ( g => g.forall(notTooClose(g, _)))).toSet
  println("grid4x7bottomLeft1Shouldbe0.size " + grid4x7bottomLeft1Shouldbe0.size)
  printGrid(grid4x7bottomLeft1.head)
  printGrid(grid4x7bottomLeft1Shouldbe0.head)
  val /* def*/ grid4x7bottomLeft2 =          (grid4x7bottomLeft2FullGrid.subsets(5).map(g => g /*+ ((8,8))*/).filter ( g => g.forall(notTooClose(g, _)))).withFilter(holes =>
         findOneTwoThrees((grid4x7bottomLeft2FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
          case Some(x) => true
          case _ => false
         }).toStream
  printGrid(grid4x7bottomLeft2.head)
                                                  
  println("grid4x7bottomLeft2.size" + grid4x7bottomLeft2.size)
  val grid4x7bottomLeft2Shouldbe0 = (grid4x7bottomLeft2FullGrid.subsets(6).map(g => g /*+ ((8,8))*/).filter ( g => g.forall(notTooClose(g, _)))).toSet
  println("grid4x7bottomLeft2Shouldbe0.size " + grid4x7bottomLeft2Shouldbe0.size)
  printGrid(grid4x7bottomLeft2Shouldbe0.head)
  
/*  for {
   grid4x7bottomLeft1filtered <- grid4x7bottomLeft1
   grid4x7bottomLeft2filtered <- grid4x7bottomLeft2
   if (grid4x7bottomLeft1filtered.forall(notTooClose(grid4x7bottomLeft2filtered, _)))
   val gridBottomLeftCombined = (grid4x7bottomLeft1filtered ++ grid4x7bottomLeft2filtered)
   if (findOneTwoThrees((grid8x7LeftFullGrid -- gridBottomLeftCombined).map(x=> x -> Set(1,2,3)).toMap, Map(), gridBottomLeftCombined) match {
   case Some(x) => true
   case _ => false
  })
  } yield gridBottomLeftCombined
  */
  
  val /*def*/ grid4x7bottomLeft1filtered = grid4x7bottomLeft1.filter(holes => findOneTwoThrees((grid4x7bottomLeft1FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => true
   case _ => false
  })
  println("grid4x7bottomLeft1filtered.size " + grid4x7bottomLeft1filtered.size)
  printGrid(grid4x7bottomLeft1filtered.head)
  
  val /* def*/ grid4x7bottomLeft2filtered = grid4x7bottomLeft2.filter(holes => findOneTwoThrees((grid4x7bottomLeft2FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => true
   case _ => false
  })
 printGrid(grid4x7bottomLeft2filtered.head)
  println("grid4x7bottomLeft2filtered.size " + grid4x7bottomLeft2filtered.size)
  
  val gridBottomLeftCombinedSize10 = for {
     bl1 <- grid4x7bottomLeft1filtered
     bl2 <- grid4x7bottomLeft2filtered
     if (bl1.forall(notTooClose(bl2, _)))
    } yield bl1 ++ bl2

  val gridBottomLeftCombinedSize11 = (for {
     bl1 <- grid4x7bottomLeft1Size6
     bl2 <- grid4x7bottomLeft2filtered
     if (bl1.forall(notTooClose(bl2, _)))
    } yield bl1 ++ bl2) ++ (for {
     bl1 <- grid4x7bottomLeft1filtered
     bl2 <- grid4x7bottomLeft2Size6
     if (bl1.forall(notTooClose(bl2, _)))
    } yield bl1 ++ bl2)
    
    
    
    
  printGrid(gridBottomLeftCombinedSize10.head)
  println("gridBottomLeftCombined10.size " + gridBottomLeftCombinedSize10.size)
   
  val gridBottomLeftCombinedSize10Filtered = gridBottomLeftCombinedSize10.filter(holes =>
  !listAllOneTwoThrees((grid8x7LeftFullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes).isEmpty)

  val gridBottomLeftCombinedSize11Filtered = gridBottomLeftCombinedSize11.filter(holes =>
  !listAllOneTwoThrees((grid8x7LeftFullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes).isEmpty)
  printGrid(gridBottomLeftCombinedSize11Filtered.head)
  
  
  /*findOneTwoThrees((grid8x7LeftFullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => true
   case _ => false
  })*/
  println("gridBottomLeftCombinedSize10Filtered.size 7744 " + gridBottomLeftCombinedSize10Filtered.size)
  println("gridBottomLeftCombinedSize11Filtered.size ???? " + gridBottomLeftCombinedSize11Filtered.size)

  val /*def*/ gridfull15 = for {
   (bottomLeft, i) <- gridBottomLeftCombinedSize11Filtered.zipWithIndex.toIterator
   // 90 degree clockwise rotation
  // val topLeft = bottomLeft.map( bottomLeftElement => ( bottomLeftElement._2 , 16 - bottomLeftElement._1))
   topLeft <- gridBottomLeftCombinedSize10Filtered/*.drop(i)*/.map(_.map( bottomLeftElement => ( bottomLeftElement._2 , 16 - bottomLeftElement._1)))
   if (bottomLeft.forall(notTooClose(topLeft, _ )) && !hasCliqueOfSize4(bottomLeft ++ topLeft))
   // Then by symmetry bottomLeft will be not too close to bottom right as well.
   // similar argument to bottomright and topright.
   bottomRight <- gridBottomLeftCombinedSize10Filtered.map(_.map(bottomLeftElement => ( 16 - bottomLeftElement._2 , bottomLeftElement._1)))
   //val bottomRight = topLeft.map(topLeftElement => ( 16 - topLeftElement._1 , 16 - topLeftElement._2))
   if (bottomRight.forall(notTooClose(bottomLeft ++ topLeft, _ )) && !hasCliqueOfSize4(bottomLeft ++ bottomRight ++ topLeft))
   topRight <- gridBottomLeftCombinedSize10Filtered.map(_.map(bottomLeftElement => ( 16 - bottomLeftElement._1 , 16 - bottomLeftElement._2)))
  // val topRight    = bottomRight.map(bottomRightElement => ( 16 - bottomRightElement._2 , bottomRightElement._1))
   if (topRight.forall(notTooClose(bottomLeft ++ topLeft ++ bottomRight, _ )) && !hasCliqueOfSize4(bottomLeft ++ bottomRight ++ topLeft ++ topRight ))
  } yield bottomLeft ++ topLeft ++ bottomRight ++ topRight
 //printGrid(gridfull15.next)
  //println("gridfull15.size " + gridfull15.size)
 println("got full grid, not printing size")

/*
  /*val*/ def gridLeftSide = for {
   bottomLeft <- gridBottomLeftCombinedFiltered
   // 90 degree clockwise rotation
   topLeft <- gridBottomLeftCombinedFiltered.map(_.map( bottomLeftElement => ( bottomLeftElement._2 , 16 - bottomLeftElement._1)))
   if (bottomLeft.forall(notTooClose(topLeft, _ )))
  } yield bottomLeft ++ topLeft
  println("gridLeftSide.size " + gridLeftSide.size)
  

  /*val*/ def gridLeftSideFiltered = gridLeftSide.filter(holes => findOneTwoThrees((grid7point5x15LeftFullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes) match {
   case Some(x) => true
   case _ => false
  })
  println("gridLeftSideFiltered.size " + gridLeftSideFiltered.size)
*/
/*
  /*val*/ def gridfull15 = for {
   leftSide <- gridLeftSideFiltered
   // 180 degree clockwise rotation
   rightSide <- gridLeftSideFiltered.map(_.map( leftElement => ( 16 - leftElement._2 , 16 - leftElement._1)))
  if (leftSide.forall(notTooClose(rightSide, _ )))
  } yield leftSide ++ rightSide
 println("gridfull15.size " + gridfull15.size)
*/
  println("starting to print Solutions")
  
  /*
  Solution
2.313.2113.232.
.31.323.2113.21
121211223.23132
2231.31122123.2
.13221.313.2313
321.3221.32123.
2.131.31213.211
332213231.323.2
21.321.31211221
.132313231.313.
321.321.3221.32
2.132.131.31213
3321332213231.3
.12121.321.3221
2.12.132.13131.
counts Map(2 -> 62, 1 -> 62, 3 -> 60)
Solution
2.313.2113.232.
.31.323.2113.21
121211223.23132
2231.31122123.2
.13221.313.2313
321.3221.32123.
2.131.31213.211
332213231.323.2
21.321.31211221
.132213231.313.
321.321.3221.32
2.132.131.31213
3321332213231.3
.12121.321.3221
2.13.132.13131.
counts Map(2 -> 62, 1 -> 62, 3 -> 60)
Solution
2.313.2113.232.
.31.323.2113.21
121211223.23132
2231.31122123.2
.13221.313.2313
321.3221.32123.
2.131.31213.211
332213231.323.2
21.321.31211221
.132313231.313.
321.321.3221.32
2.132.131.31213
3321332213231.3
.12121.321.3221
2.13.132.13121.
counts Map(2 -> 62, 1 -> 62, 3 -> 60)
Solution
2.313.2113.232.
.31.323.2113.21
121211223.23132
2231.31122123.2
.13221.313.2313
321.3221.32123.
2.131.31213.211
332213231.323.2
21.321.31211221
.132313231.313.
321.321.3221.32
2.132.131.31213
3321332213231.3
.12121.321.3221
2.13.132.13131.
counts Map(2 -> 61, 1 -> 62, 3 -> 61)
Solution
2.313.2113.232.
.31.323.2113.21
121211223.23132
2231.31122123.2
.13221.313.2313
321.3221.32123.
2.131.31213.211
332213231.323.2
21.321.31211221
.132313231.313.
321.321.3221.32
2.132.131.31213
3321332213231.3
.12121.321.3221
2.32.132.13131.
counts Map(2 -> 62, 1 -> 61, 3 -> 61)
  

  
  */
  var trialsCount = 0
  gridfull15.flatMap{holes =>
  if (trialsCount % 1 == 0) {
  print("\r" + trialsCount)
//   printGrid(holes)
  }
  trialsCount+=1
  listAllOneTwoThrees((grid15x15FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes)}
  .foreach{ sol =>  //printGridPath)
  println("Solution")
  printGridPath(sol)
  println("counts "  + sol.values.groupBy(identity).mapValues(_.size))
  }
  println("done printing solutions")

  val/* def*/ full15sFiltered =
  gridfull15.withFilter(holes => /*findOneTwoThrees*/
   !listAllOneTwoThrees((grid15x15FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes).isEmpty) // match {
 /*  case Iterator(x) => true
   case _ => false
   })*/
   println("filtered full grid, not printing size")
//   println("full15filtered size " + full15sFiltered.size)
 /*
   full15sFiltered.foreach{ x =>
    println("Full15filtered hole " + hasCliqueOfSize4(x))
    println(x)
    printGrid(x)
   }
   */
   /*
   val sols15 = full15sFiltered.map{ holes => (holes, findOneTwoThrees((grid15x15FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes))}
   val sols215 = sols15.map( x => (x, x._2.get.map(x => x._2).groupBy(identity).mapValues(_.size)))
   
   println("here1 at solution")
   val totalSol15 = sols215.withFilter(possibleSolution => possibleSolution._2.values.max  == 62)
   println("here2 at solution")
   totalSol15.foreach{x =>
   println("Solution")
   printGridPlacedAndHoles(x._1._2.get, x._1._1)}
   */
   /*
   println("here3 at solution")
   val solFull =full15sFiltered.map { holes => ( holes, listAllOneTwoThrees((grid15x15FullGrid -- holes).map(x=> x -> Set(1,2,3)).toMap, Map(), holes))}
   println("here4 at solution")
   val totalSol152 = solFull.map(possibleSolution => (possibleSolution._1, possibleSolution._2.filter(m => m.values.max  == 62)))
   totalSol152.foreach{ xx => xx._2.foreach{ x =>
    println("Solution")
    printGridPlacedAndHoles(x, xx._1)}}
*/

//        rightHalf <- left5x10sfiltered.map(_.map(lefthalfElement => (11- lefthalfElement._1, lefthalfElement._2)))
   
 /*
 Set((7,6), (10,5), (8,9), (6,10), (5,1), (6,4), (10,10), (1,6), (10,1), (2,8), (1,1), (1,10), (4,5), (4,9), (5,7), (9,7), (3,2), (2,4), (9,3), (7,2))
o....o...o
...o...o..
.o........
....o...o.
o.....o...
...o.....o
.o...o....
........o.
..o...o...
o...o....o
Full10 hole false
Set((7,5), (1,5), (7,9), (6,7), (3,9), (6,1), (5,10), (10,10), (10,1), (1,1), (1,10), (4,6), (9,8), (8,2), (5,4), (10,6), (2,7), (4,2), (2,3), (9,4))
o...o....o
..o...o...
........o.
.o...o....
...o.....o
o.....o...
....o...o.
.o........
...o...o..
o....o...o
full10filtered size 0
 */


  /*  for { holeNumber <- List(10, 11, 12, 13) } {
      databaseOfErrors.clear()
      println("here1a + holeNumber " + holeNumber)
      val leftHalfsIterator = createHoles(Set(), grid5x10, Set((1, 1), (2, 1), (3, 1), (4, 1), (5, 1)), holeNumber) ///*.take(100)*/.foreach(printGrid)

      println("here1b iterator size ")
      //println(leftHalfsIterator.size )
      // Deduplicate -- EXPENSIVE!
      val leftHalfsunfilter = leftHalfsIterator.toSet
      println("Set unfilter " + leftHalfsunfilter.size)
      val leftHalfs = leftHalfsunfilter
        .filter(holes => findOneTwoThrees((grid5x10 -- holes).map(x => x -> Set(1, 2, 3)).toMap, Map(), holes) match {
          case Some(x) => true
          case _       => false
        })

      println("here1c ")
      val full10copypair = for {
        leftHalfouter <- leftHalfs
        rightHalf <- leftHalfs.map(_.map(lefthalfElement => (lefthalfElement._1 + 5, lefthalfElement._2)))
        if (leftHalfouter.forall(notTooClose(rightHalf, _)))
      } yield (leftHalfouter, rightHalf)

      val full10copy = full10copypair.map { case (a, b) => a ++ b }

      val full10reflectx = for {
        leftHalfouter <- leftHalfs
        rightHalf <- leftHalfs.map(_.map(lefthalfElement => (11 - lefthalfElement._1, lefthalfElement._2)))
        if (leftHalfouter.forall(notTooClose(rightHalf, _)))
      } yield (leftHalfouter ++ rightHalf)

      val full10reflecty = for {
        leftHalfouter <- leftHalfs
        rightHalf <- leftHalfs.map(_.map(lefthalfElement => (lefthalfElement._1 + 5, 11 - lefthalfElement._2)))
        if (leftHalfouter.forall(notTooClose(rightHalf, _)))
      } yield (leftHalfouter ++ rightHalf)

      val full10rot180 = for {
        leftHalfouter <- leftHalfs
        rightHalf <- leftHalfs.map(_.map(lefthalfElement => (11 - lefthalfElement._1, 11 - lefthalfElement._2)))
        if (leftHalfouter.forall(notTooClose(rightHalf, _)))
      } yield (leftHalfouter ++ rightHalf)

      val full10 = full10copy ++ full10reflectx ++ full10reflecty ++ full10rot180

      println("here2 lefthalfs.size " + leftHalfs.size + " full10.size " + full10.size + " copy " + full10copy.size + " x " + full10reflectx.size + " y " + full10reflecty.size + " rot180 " + full10rot180.size)

      full10.foreach { x =>
        println("Full10 hole " + hasCliqueOfSize4(x))
        println(x)
        printGrid(x)
      }
      full10copy.foreach { x =>
        println("Full10copy hole " + hasCliqueOfSize4(x) + " size " + x.size)
        println(x)
        printGrid(x)
      }
      full10copypair.foreach { x =>
        println("Full10copypair hole size " + x._1.size + " + " + x._2.size)
        println(x)
        println("left")
        printGrid(x._1)
        println("right")
        printGrid(x._2)
      }

      full10reflectx.foreach { x =>
        println("Full10x hole " + hasCliqueOfSize4(x))
        println(x)
        printGrid(x)
      }
      full10reflecty.foreach { x =>
        println("Full10y hole " + hasCliqueOfSize4(x))
        println(x)
        printGrid(x)
      }

      full10.flatMap(holes => findOneTwoThrees((grid10x10 -- holes).map(x => x -> Set(1, 2, 3)).toMap, Map(), holes)).foreach { x =>
        println("Sol from full10")
        printGridPath(x)
      }

      // Now to do 9 and 11!!!
      println("Finished 510")
    }
    val leftHalfsIterator119 = createHoles(Set(), grid5x10, Set((1, 1), (2, 1), (3, 1), (4, 1), (5, 1)), 11) ///*.take(100)*/.foreach(printGrid)

    databaseOfErrors.clear()
    println("Testempty " + databaseOfErrors.contains(Set()))
    /*val sols = createHoles(Set(), grid10, Set((3, 1))) //.take(100).foreach(printGrid)
    println("here1")
    sols.flatMap(holes => findOneTwoThrees((grid10 -- holes).map(x => x -> Set(1, 2, 3)).toMap, Map(), holes)).foreach { x =>
      println("Sol")
      printGridPath(x)
    }
    */
    */
  }
}