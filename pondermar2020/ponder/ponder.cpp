#include <iostream>
#include <utility>
#include <bits/stdc++.h> 
using namespace std;
struct Point{int x,y;} ;

int mapOffsetx [16] = { 
    -2, -1 , 1, 2,
    -2, -1 , 1, 2,
    -2, -1 , 1, 2,
    0,0,0,0};
int mapOffsety [16] = { 
    -2, -1 , 1, 2,
    0,0,0,0,
    2,  1 , -1, -2,
    -2, -1 , 1, 2,
    };

    
void mapOffset(Point& p, Point outPoints[])
{
    for (int i = 0; i < 16 ; i++)
    {
        outPoints[i].x = p.x + mapOffsetx[i];
        outPoints[i].y = p.y + mapOffsety[i];
    }
}
    
 /*   
    def mapOffset(point: (Int, Int)) =
      {
        val offSet = Set((-2, -2), (-1, -1), (1, 1), (2, 2), // up and to the right
          (-2, 0), (-1, 0), (1, 0), (2, 0), // horizontal
          (-2, 2), (-1, 1), (1, -1), (2, -2), // down and to the right
          (0, -2), (0, -1), (0, 1), (0, 2) // vertical
        )
        offSet.map(o => (o._1 + point._1, o._2 + point._2))
      }
      */
 
int mapOffsetKnightx[16] = {
    -2, -2, 2, 2,
    -1, -1, 1, 1};
int mapOffsetKnighty[16] = {
   -1, 1, -1, 1,
   -2, 2, -2, 2
};
 
void mapOffsetKnight(Point& p, Point outPoints[])
{
       for (int i = 0; i < 16 ; i++)
    {
        outPoints[i].x = p.x + mapOffsetKnightx[i];
        outPoints[i].y = p.y + mapOffsetKnighty[i];
    }
}
    
bool notTooClose(unordered_set <Point> holesSoFar, Point p)
{
  !holesSoFar.contains(p) ;  
}
    
    def notTooClose(holesSoFar: Set[(Int, Int)], point: (Int, Int)): Boolean =
      !holesSoFar.exists(mapOffset(point).contains)

    def printGrid(grid: Set[(Int, Int)]) =
      (10 to 1 by -1).map { y =>
        println((1 to 10).map { x => if (grid.contains((x, y))) "o" else "." }.mkString)
      }

    val grid10 = (for {
      x <- 1 to 10
      y <- 1 to 10
    } yield (x, y)).toSet
    val grid5 = (for {
      x <- 1 to 5
      y <- 1 to 5
    } yield (x, y)).toSet
    val grid510 = (for {
      x <- 1 to 5
      y <- 1 to 10
    } yield (x, y)).toSet
    val grid105 = (for {
      x <- 1 to 10
      y <- 1 to 7
    } yield (x, y)).toSet

    val grid3Quarters = (for {
      x <- 1 to 5
      y <- 1 to 10
    } yield (x, y)).toSet ++
      (for {
        x <- 6 to 10
        y <- 1 to 5
      } yield (x, y)).toSet

    def printGridPath(sol: Map[(Int, Int), Int]) =
      (10 to 1 by -1).map { y =>
        println((1 to 10).map { x =>
          sol.get((x, y)) match {
            case Some(c) => c //.toChar
            case None    => "."
          }
        }.mkString)
      }

    def printGridPlacedAndHoles(placed: Map[(Int, Int), Int], holes: Set[(Int, Int)]) =
      (10 to 1 by -1).map { y =>
        println((1 to 10).map { x =>
          placed.get((x, y)) match {
            case Some(c) => c //.toChar
            case None    => if (holes.contains((x, y))) "o" else "."
          }
        }.mkString)
      }

    // So we could represent the candidates within the Set(1,2,3) as a binary number from 1 to 7. It would be 0 if it is empty.
    def printGridRemaining(sol: Map[(Int, Int), Set[Int]]) =
      (10 to 1 by -1).map { y =>
        println((1 to 10).map { x =>
          sol.get((x, y)) match {
            case Some(c) => c.map(cand => math.pow(2, (cand - 1))).sum.toInt //.toChar
            case None    => "."
          }
        }.mkString)
      }

    def updateRemaining(newPlacementPosition: (Int, Int), newValue: Int, remaining: Map[(Int, Int), Set[Int]], holes: Set[(Int, Int)], placed: Map[(Int, Int), Int]): Map[(Int, Int), Set[Int]] =
      {
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
            case None                  => None
          }
        }
        remaining ++ remainingPositionsAndValuesToUpdate - newPlacementPosition
      }

    def neighbours(holes: Set[(Int, Int)], point: (Int, Int)) = {
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

    var findCount = 0
    def findOneTwoThrees(remaining: Map[(Int, Int), Set[Int]], placed: Map[(Int, Int), Int], holes: Set[(Int, Int)]): Option[Map[(Int, Int), Int]] = remaining.size match {
      case 0 => Some(placed) // Some(Map()) // Some(placed) // Maybe Some( Map() ) and do the recursive path building thing...
      case _ => {
        val lowestCandidateCount = remaining.filter(cell => cell._2.size == remaining.minBy { _._2.size }._2.size).maxBy(cell => neighbours(holes, cell._1).size)
        if (findCount % 100000000 == 0) {
          println("findCount " + findCount)
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
        }
        findCount = findCount + 1

        def findLoop(candidates: Set[Int]): Option[Map[(Int, Int), Int]] = candidates.headOption match {
          case None => None
          case Some(candidate) => findOneTwoThrees(updateRemaining(lowestCandidateCount._1, candidate, remaining, holes, placed), placed.updated(lowestCandidateCount._1, candidate), holes) match {
            case Some(solution) => Some(solution) // Some(solution.updated(lowestCandidateCount._1, candidate))
            case None           => findLoop(candidates - candidate)
          }
        }
        // Without loss of generality, the first two placements can be "1" and "2" respectively.
        if (placed.size <= 1)
          findLoop(Set(lowestCandidateCount._2.head))
        else
          findLoop(lowestCandidateCount._2)
      }
    }

    val holesTest = Set((1, 1), (5, 1), (3, 2), (4, 4), (8, 4), (2, 5), (6, 5), (9, 6), (1, 7), (7, 7), (10, 8), (2, 9), (8, 9), (6, 10))
    println("StartTest " + findCount)
    val holesTestResult = findOneTwoThrees((grid5 -- holesTest).map(x => x -> Set(1, 2, 3)).toMap, Map(), holesTest)
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

    def hasCliqueOfSize4(holes: Set[(Int, Int)]) =
      (grid5 -- holes).exists { point =>
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

    println("clique test start")
    printGrid(holesTest)
    println(hasCliqueOfSize4(holesTest))
    println("Clique test end")

    var databaseOfErrors = scala.collection.mutable.Set[Set[(Int, Int)]]()

    val lines = Source.fromFile("databaseOfErrors.txt").getLines.toSet

    //databaseOfErrors += lines.map(_.toSet)

    def isItAKnownError(holesSoFar: Set[(Int, Int)]): Boolean =
      !databaseOfErrors.exists(err => err.subsetOf(holesSoFar)) match {
        case true => {
          println("Start of known error1")
          // Add to the database of errors

          // Now, we can search for this essential error in two ways
          // 1. Subsets starting from smallest, finding the first
          // 2. Subsets of size (n-1), recursively, finding the last.
          // So 1 will involve about half of 2^size successful finds before we get the first error. "Finds" are quick though.
          // 2 will only involve a quadratic number of fails, but the fails are probably exponetial
          //   is approximately the central binomial coeffiencents, so just under exponential.
          val essentialError = holesSoFar.subsets.map(sub => (sub, findOneTwoThrees((grid10 -- sub).map(x => x -> Set(1, 2, 3)).toMap, Map(), sub))).find { sub =>
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

    var createHolesCount = 0
    val n = 15

    def createHoles(holesSoFar: Set[(Int, Int)], candidates: Set[(Int, Int)], knightMoves: Set[(Int, Int)]): Iterator[Set[(Int, Int)]] = holesSoFar.size match {
      case 20 => { /// ******** FFFIXXXXX MEEEE!!! return to 20
        //println("Solution ...")
        // printGrid(holesSoFar)
        Iterator(holesSoFar)
      }
      case s if s < 2 => {
        val goodCandidates = candidates.filter(notTooClose(holesSoFar, _))
        if (createHolesCount % 1000000000 == 0) {
          println("createholes count " + createHolesCount)
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

        if (candidates.size + holesSoFar.size >= 20 && !hasCliqueOfSize4(holesSoFar) )//&& isItAKnownError(holesSoFar))
          /*findOneTwoThrees((grid -- holesSoFar).map( x => x -> Set(1,2,3)).toMap , Map(), holesSoFar) match {
     case Some(a) => { */
          for {
            goodCandidate <- knightMoves.toIterator
            val nextCandidateSet = candidates -- mapOffset(goodCandidate) - goodCandidate
            holes <- createHoles(holesSoFar + goodCandidate, nextCandidateSet, (knightMoves ++ mapOffsetKnight(goodCandidate)) intersect nextCandidateSet)
          } yield holes
        // }  // case Some(a) => {
        //case _ => { // findOneTwoThree returned None, meaning there's no way to place 1 2 or 3s, even with no further holes!
        //     Iterator()
        //   } // case _ => {
        // } // match (findOneTwoThrees
        else {

          Iterator() // There's no possible way to solve
        }
      } // case s if s < 15 => {
      case _ => {
        if (createHolesCount % 100000000 == 0) {
          println("s> 2 createholes count " + createHolesCount)
          println("HolesSo far ", holesSoFar.size)
          printGrid(holesSoFar)
          println("candidates ", candidates.size)
          printGrid(candidates)
          println("knightmoves " + knightMoves.size)
          printGrid(knightMoves)
        }
        createHolesCount = createHolesCount + 1

        if (candidates.size + holesSoFar.size >= 20 && !hasCliqueOfSize4(holesSoFar))// && isItAKnownError(holesSoFar))
          for {
            goodCandidate <- candidates.toIterator
            val nextCandidateSet = candidates -- mapOffset(goodCandidate) - goodCandidate
            holes <- createHoles(holesSoFar + goodCandidate, nextCandidateSet, (knightMoves ++ mapOffsetKnight(goodCandidate)) intersect nextCandidateSet)
          } yield holes
        else
          Iterator()
      }
    }
    // Creates the empty Stream as it should (because KnightMoves is empty).
    createHoles(Set(), Set(), Set()).take(2)
    createHoles(Set(), Set((1, 1)), Set((1, 1)))

    grid10.filter(notTooClose(Set(), _))
    grid10.filter(notTooClose(Set(), _)).size
    grid10.filter(notTooClose(Set((1, 1)), _))
    grid10.filter(notTooClose(Set((1, 1)), _)).size

    grid10.filter(notTooClose(Set((1, 1), (7, 3)), _)).size
    printGrid(grid10)
    printGrid(grid10.filter(notTooClose(Set((1, 1), (7, 3)), _)))

    //createHoles( Set(), grid).foreach(printGrid)

    val mostlyThere = Set((2, 1), (4, 2), (6, 3), (8, 4), (10, 5),
      (1, 3), (3, 4), (5, 5), (7, 6), (9, 7),
      (2, 6), (4, 7), (6, 8), (8, 9))

    println("mostly there test for clique")
    printGrid(mostlyThere)
    print(hasCliqueOfSize4(mostlyThere))
    println("end!!")
    val mostlySolution = findOneTwoThrees((grid10 -- mostlyThere).map(x => x -> Set(1, 2, 3)).toMap, Map(), mostlyThere)
    print(mostlySolution)
    /*createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere).foreach{sol =>
 println("Sol")
 printGrid(sol)}
  */

    ////****** We also need to consider neighbours of newPlacementPosition (non holes  --- in the "placed"), since they could cause an accidental win.

    //  val neighbours = mapOffset(newPlacementPosition) intersect placed.toList filter (x => // Huh, I'm surprised this works intersect of a Set and a Map???

    val tHoles = Set((1, 1), (7, 3)) // grid.filter( notTooClose(Set((1,1),(7,3)), _))
    printGrid(tHoles)
    val t = (grid10 -- tHoles).map(x => x -> Set(1, 2, 3)).toMap
    printGridRemaining(updateRemaining((6, 2), 2, t, tHoles, Map()))

    tHoles.size
    printGrid(grid10 -- tHoles)
    (grid10 -- tHoles).size

    updateRemaining((6, 2), 2, t, tHoles, Map())
    printGridRemaining(updateRemaining((6, 2), 2, t, tHoles, Map()))
    updateRemaining((6, 2), 2, t, tHoles, Map()).size
    updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2))
    printGridRemaining(updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2)))
    updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2)).size

    val effected = updateRemaining((6, 2), 2, t, tHoles, Map()).filter { case (k, v) => v.size == 2 }
    val effected2 = updateRemaining((6, 2), 2, t, tHoles, Map((6, 3) -> 2)).filter { case (k, v) => v.size == 2 }

    printGridPath(Map((1, 1) -> 1, (4, 1) -> 2, (7, 3) -> 3))
    createHoles(mostlyThere, grid10 -- mostlyThere.flatMap(mapOffset) -- mostlyThere, Set()).take(3).toList.map(holes =>
      findOneTwoThrees((grid10 -- holes).map(x => x -> Set(1, 2, 3)).toMap, Map(), holes))

    // See if we can find a solution with no constrainst on holes!
    val basicSolution = findOneTwoThrees(grid5.map(x => x -> Set(1, 2, 3)).toMap, Map(), Set())
    printGridPath(basicSolution.get)
    val xeu = basicSolution.get.map(x => x._2).groupBy(identity).mapValues(_.size)

    //createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere).take(200).toList.flatMap(holes => findOneTwoThrees( (grid -- holes).map( x => x -> Set(1,2,3)).toMap , Map(), holes) )
    //val solsTest = createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere)./*toList.*/flatMap(holes => findOneTwoThrees( (grid -- holes).map( x => x -> Set(1,2,3)).toMap , Map(), holes) )
    //solsTest.foreach{ printGridPath}

    //createHoles( mostlyThere, grid -- mostlyThere.flatMap(mapOffset) -- mostlyThere).size
    List(Some(1), Some(3), None, Some(5), None).flatMap { x => x }

    val fourByfour = (for {
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

    val sols = createHoles(Set(), grid10, Set((3, 1))) //.take(100).foreach(printGrid)
    println("here1")
    sols.flatMap(holes => findOneTwoThrees((grid10 -- holes).map(x => x -> Set(1, 2, 3)).toMap, Map(), holes)).foreach { x =>
      println("Sol")
      printGridPath(x)
    }
  }                                               //> main: (args: Array[String])Unit
}


int main()
{
 

  
  return 0;

}
