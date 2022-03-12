object ponder {
/*
https://www.research.ibm.com/haifa/ponderthis/challenges/July2019.html
A non-intersecting knight's cycle is a closed tour made of legal moves of a chess knight on a rectangular board of size mXn, such that the straight lines connecting consecutively visited fields make a simple polygon of an enclosed area A.
Find two non-intersecting knight's cycles of length 14 on boards of size <= 40 such that the difference between their areas is at least 7.
*/

 (1 to 49)                                        //> res0: scala.collection.immutable.Range.Inclusive = Range 1 to 49
 (1 to 49).toSet                                  //> res1: scala.collection.immutable.Set[Int] = Set(5, 10, 42, 24, 37, 25, 14, 2
                                                  //| 0, 46, 29, 1, 6, 28, 38, 21, 33, 9, 13, 41, 2, 32, 34, 45, 17, 22, 44, 27, 1
                                                  //| 2, 49, 7, 39, 3, 35, 48, 18, 16, 31, 11, 43, 40, 26, 23, 8, 36, 30, 19, 4, 4
                                                  //| 7, 15)
 (1 to 49).toSet.subsets(6).take(10).foreach(println)
                                                  //> Set(5, 10, 42, 24, 37, 25)
                                                  //| Set(5, 10, 42, 24, 37, 14)
                                                  //| Set(5, 10, 42, 24, 37, 20)
                                                  //| Set(5, 10, 42, 24, 37, 46)
                                                  //| Set(5, 10, 42, 24, 37, 29)
                                                  //| Set(5, 10, 42, 24, 37, 1)
                                                  //| Set(5, 10, 42, 24, 37, 6)
                                                  //| Set(5, 10, 42, 24, 37, 28)
                                                  //| Set(5, 10, 42, 24, 37, 38)
                                                  //| Set(5, 10, 42, 24, 37, 21)
 (1 to 49).toSet.subsets(6).size                  //> res2: Int = 13983816
 (1 to 49).toSet.subsets(6).filter( _.count( _ % 2 == 0) ==3).take(10).foreach(println)
                                                  //> Set(5, 10, 42, 24, 37, 25)
                                                  //| Set(5, 10, 42, 24, 37, 29)
                                                  //| Set(5, 10, 42, 24, 37, 1)
                                                  //| Set(5, 10, 42, 24, 37, 21)
                                                  //| Set(5, 10, 42, 24, 37, 33)
                                                  //| Set(5, 10, 42, 24, 37, 9)
                                                  //| Set(5, 10, 42, 24, 37, 13)
                                                  //| Set(5, 10, 42, 24, 37, 41)
                                                  //| Set(5, 10, 42, 24, 37, 45)
                                                  //| Set(5, 10, 42, 24, 37, 17)
 (1 to 49).toSet.subsets(6).filter( _.count( _ % 2 == 0) ==3).size
                                                  //> res3: Int = 4655200

 
 //double a[7] = {0, 1, 1, 1, 0, 1, 1};
 val a2 = List(0, 1, 1, 1, 0, 1, 1)               //> a2  : List[Int] = List(0, 1, 1, 1, 0, 1, 1)
 a2.groupBy(identity)                             //> res4: scala.collection.immutable.Map[Int,List[Int]] = Map(1 -> List(1, 1, 1,
                                                  //|  1, 1), 0 -> List(0, 0))
 a2.groupBy(identity).mapValues(_.size)           //> res5: scala.collection.immutable.Map[Int,Int] = Map(1 -> 5, 0 -> 2)
 a2.groupBy(identity).mapValues(_.size).head      //> res6: (Int, Int) = (1,5)
   // or maxBy(_._1)
 a2.groupBy(identity).mapValues(_.size).maxBy(_._1)
                                                  //> res7: (Int, Int) = (1,5)
 2 * a2.groupBy(identity).mapValues(_.size).maxBy(_._1)._2 - a2.size
                                                  //> res8: Int = 3


  val knightMoves = Set(
   (1,  2),  (2,1),
   (-1, 2), (-2,1),
   (-1,-2), (-2,-1),
   (1, -2),  (2,-1))                              //> knightMoves  : scala.collection.immutable.Set[(Int, Int)] = Set((2,-1), (-2
                                                  //| ,1), (1,-2), (-2,-1), (-1,2), (1,2), (2,1), (-1,-2))
  knightMoves.size                                //> res9: Int = 8
 // all possible knight moves from starting position p, on board sized mXn. With (1 to m) by (1 to n) coordinates.
 def generateMovesFrom(p: (Int,Int), m: Int, n: Int): Set[(Int,Int)] = {
  knightMoves.map(move => (p._1 + move._1 , p._2 + move._2)).filter(p=> p._1 >= 1 && p._1 <= m && p._2 >= 1 && p._2 <= n )
 }                                                //> generateMovesFrom: (p: (Int, Int), m: Int, n: Int)Set[(Int, Int)]
 generateMovesFrom((1,2), 4, 10)                  //> res10: Set[(Int, Int)] = Set((3,1), (2,4), (3,3))

 // Return the coordinates of the intersection of two lines (the lines are defined by 2 points each).
 def findIntersection(l1: List[(Int,Int)], l2: List[(Int,Int)]): (Double,Double) = {
  val a1 = l1(1)._2 - l1(0)._2
  val b1 = l1(0)._1 - l1(1)._1
  val c1 = a1 * l1(0)._1 + b1 * l1(0)._2
 
  val a2 = l2(1)._2 - l2(0)._2
  val b2 = l2(0)._1 - l2(1)._1
  val c2 = a2 * l2(0)._1 + b2 * l2(0)._2
 
  val delta = a1 * b2 - a2 * b1
  // We're not worrying about parallel lines (division by zero) here since we've already filtered that out.
  ((b2 * c1 - b1 * c2).toDouble / delta, (a1 * c2 - a2 * c1).toDouble / delta)
 }                                                //> findIntersection: (l1: List[(Int, Int)], l2: List[(Int, Int)])(Double, Doub
                                                  //| le)
 // Lines don't cross if they are parallel.
 // Otherwise, the intersection point must lie outside at least one of their bounding boxes.
 def linesSegmentsDontCross(line1: List[(Int,Int)], line2: List[(Int,Int)]): Boolean = {
  ( line1(0)._1 - line1(1)._1) * ( line2(0)._2 - line2(1)._2) == ( line1(0)._2 - line1(1)._2) * ( line2(0)._1 - line2(1)._1)  || !{
  val intersection = findIntersection(line1, line2)
  // We don't worry about >= because we're allowing it to "intesect" the last line segment (eg. the one it's appending to)
  intersection._1 > math.min(line1(0)._1, line1(1)._1) &&
  intersection._1 < math.max(line1(0)._1, line1(1)._1) &&
  intersection._2 > math.min(line1(0)._2, line1(1)._2) &&
  intersection._2 < math.max(line1(0)._2, line1(1)._2) &&
  intersection._1 > math.min(line2(0)._1, line2(1)._1) &&
  intersection._1 < math.max(line2(0)._1, line2(1)._1) &&
  intersection._2 > math.min(line2(0)._2, line2(1)._2) &&
  intersection._2 < math.max(line2(0)._2, line2(1)._2)
  }
 }                                                //> linesSegmentsDontCross: (line1: List[(Int, Int)], line2: List[(Int, Int)])B
                                                  //| oolean
 linesSegmentsDontCross(List( (1,5),(2,7)), List((1,8),(2,6)))
                                                  //> res11: Boolean = false
 linesSegmentsDontCross(List( (1,5),(2,7)), List((1,8),(3,7)))
                                                  //> res12: Boolean = true
 linesSegmentsDontCross(List( (1,5),(3,6)), List((1,8),(2,6))) // This should be TRUE!!!
                                                  //> res13: Boolean = true
 findIntersection(List( (1,5),(3,6)), List((1,8),(2,6)))
                                                  //> res14: (Double, Double) = (2.2,5.6)
 
 // When adding a new point to the tour, ensure that it already isn't in the tour, and also that the new line segment doesn't cross any of the existing ones.
 def doesntCross(m: (Int,Int), soFar: List[(Int,Int)], prepend: Boolean): Boolean = {
  !soFar.contains(m) && soFar.sliding(2).forall( lineSegment => linesSegmentsDontCross( lineSegment, List(m, if (prepend)
    soFar.head
   else
    soFar.last)))
 }                                                //> doesntCross: (m: (Int, Int), soFar: List[(Int, Int)], prepend: Boolean)Bool
                                                  //| ean
 generateMovesFrom((4,2), 4, 10).filter(move => doesntCross(move, List(),false)).subsets(2).map(_.toList).foreach(println)
                                                  //> List((3,4), (2,3))
                                                  //| List((3,4), (2,1))
                                                  //| List((2,3), (2,1))
 // Shoelace formula
 // https://en.wikipedia.org/wiki/Shoelace_formula
 // We can get away with the /2 int division because a knight move will always result in a 2x1 (even) area contribution.
 def calcArea(points: List[(Int,Int)]) =
 (points zip (points.tail :+ points.head)).map{ case (p1,p2) => p1._1 * p2._2 - p1._2 * p2._1}.sum.abs / 2
                                                  //> calcArea: (points: List[(Int, Int)])Int
 
 def allToursRecursive( pointsSoFar: List[(Int,Int)], m: Int, n: Int) : List[List[(Int,Int)]] = pointsSoFar.size match {
  case 17 =>  (generateMovesFrom(pointsSoFar.head, m, n) intersect generateMovesFrom(pointsSoFar.last, m, n)).toList
   .filter(move => doesntCross(move, pointsSoFar, true))
   .filter(move => doesntCross(move, pointsSoFar, false))
  .map( _ :: pointsSoFar)

  case _ => generateMovesFrom(pointsSoFar.head, m, n).toList.filter(move => doesntCross(move, pointsSoFar, true))
   .flatMap(pointToAdd => allToursRecursive( pointToAdd :: pointsSoFar, m, n))
 }                                                //> allToursRecursive: (pointsSoFar: List[(Int, Int)], m: Int, n: Int)List[List
                                                  //| [(Int, Int)]]
 /*
def allToursRecursive( pointsSoFar: List[(Int,Int)], m: Int, n: Int) : List[List[(Int,Int)]] = pointsSoFar.size match {
  case 17 =>  for {
   pointToAdd <- (generateMovesFrom(pointsSoFar.head, m, n) intersect generateMovesFrom(pointsSoFar.last, m, n)).toList
   .filter(move => doesntCross(move, pointsSoFar, true))
   .filter(move => doesntCross(move, pointsSoFar, false))
  } yield pointToAdd :: pointsSoFar

  case _ => for {
   pointToAdd <- generateMovesFrom(pointsSoFar.head, m, n).toList.filter(move => doesntCross(move, pointsSoFar, true))
   rest <- allToursRecursive( pointToAdd :: pointsSoFar, m, n)
  } yield rest
 }
*/

 val a = allToursRecursive(List((1,1), (3,2)), 5, 8)
                                                  //> a  : List[List[(Int, Int)]] = List(List((2,4), (4,3), (3,1), (5,2), (4,4), 
                                                  //| (5,6), (3,5), (2,7), (4,6), (5,8), (3,7), (1,8), (2,6), (3,4), (1,5), (2,3)
                                                  //| , (1,1), (3,2)), List((5,1), (4,3), (2,2), (3,4), (5,3), (4,5), (5,7), (3,8
                                                  //| ), (4,6), (2,5), (3,7), (1,8), (2,6), (1,4), (3,5), (2,3), (1,1), (3,2)), L
                                                  //| ist((5,1), (4,3), (3,5), (5,4), (4,6), (5,8), (3,7), (4,5), (2,6), (3,8), (
                                                  //| 1,7), (2,5), (1,3), (3,4), (4,2), (2,3), (1,1), (3,2)))
  a.size                                          //> res15: Int = 3
  a.map(calcArea)                                 //> res16: List[Int] = List(12, 12, 12)
  a.map(calcArea).min                             //> res17: Int = 12
  a.map(calcArea).max                             //> res18: Int = 12
  a.map( x=> calcArea(x) -> x).toMap              //> res19: scala.collection.immutable.Map[Int,List[(Int, Int)]] = Map(12 -> Lis
                                                  //| t((5,1), (4,3), (3,5), (5,4), (4,6), (5,8), (3,7), (4,5), (2,6), (3,8), (1,
                                                  //| 7), (2,5), (1,3), (3,4), (4,2), (2,3), (1,1), (3,2)))
 val allTours = for{
  m <- (2 to math.sqrt(40).toInt)
  n = 40/m
  firstPiece <- (1 to n/2).map((1,_)) // (1,1), (1,2), (1,3), (1,4), (1,5)
  List(p2, p14) <- generateMovesFrom(firstPiece, m, n).filter(move => doesntCross(move, List(),                                                 false)).subsets(2).map(_.toList)
  p3  <-           generateMovesFrom(p2, m,n)         .filter(move => doesntCross(move, List(p14,                 firstPiece,p2),               false))
  p13 <-           generateMovesFrom(p14,m,n)         .filter(move => doesntCross(move, List(p14,                 firstPiece,p2,p3),            true))
  p4  <-           generateMovesFrom(p3, m,n)         .filter(move => doesntCross(move, List(p13,p14,             firstPiece,p2,p3),            false))
  p12 <-           generateMovesFrom(p13,m,n)         .filter(move => doesntCross(move, List(p13,p14,             firstPiece,p2,p3,p4),         true))
  p5  <-           generateMovesFrom(p4, m,n)         .filter(move => doesntCross(move, List(p12,p13,p14,         firstPiece,p2,p3,p4),         false))
  p11 <-           generateMovesFrom(p12,m,n)         .filter(move => doesntCross(move, List(p12,p13,p14,         firstPiece,p2,p3,p4,p5),      true))
  p6  <-           generateMovesFrom(p5, m,n)         .filter(move => doesntCross(move, List(p11,p12,p13,p14,     firstPiece,p2,p3,p4,p5),      false))
  p10 <-           generateMovesFrom(p11,m,n)         .filter(move => doesntCross(move, List(p11,p12,p13,p14,     firstPiece,p2,p3,p4,p5,p6),   true))
  p7  <-           generateMovesFrom(p6, m,n)         .filter(move => doesntCross(move, List(p10,p11,p12,p13,p14, firstPiece,p2,p3,p4,p5,p6),   false))
  p9  <-           generateMovesFrom(p10,m,n)         .filter(move => doesntCross(move, List(p10,p11,p12,p13,p14, firstPiece,p2,p3,p4,p5,p6,p7),true))
  p8  <-          (generateMovesFrom(p7,m,n) intersect generateMovesFrom(p9,m,n)).filter(move =>
  (doesntCross(move, List(p9,p10,p11,p12,p13,p14, firstPiece,p2,p3,p4,p5,p6,p7),false)) && // We are both prepending and appending this move onto the list, completing the tour.
   doesntCross(move, List(p9,p10,p11,p12,p13,p14, firstPiece,p2,p3,p4,p5,p6,p7),true))
}
yield List(firstPiece,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14)
 
 allTours.size
 def printChessNotation(prize: List[(Int,Int)]) =
  (prize :+ prize.head).foreach{ case (i,j) => print(('a' to 'z')(i-1) + j.toString + ' ') }
 def printWinner(prize: List[(Int,Int)]) = {
  val lines = for (j <-10 to 1 by -1)
   yield (1 to 10).map(i => if(prize.indexOf((i,j)) != -1) "%02d".format(prize.indexOf((i,j))+1) else "  " ).mkString
  lines.foreach(println)
 }
 val immMap = allTours.map( x => calcArea(x) -> x ).toMap
 println(immMap.minBy(_._1)._1)
 printChessNotation(immMap.minBy(_._1)._2)
 printWinner(immMap.minBy(_._1)._2)
 /*
 octave:
 pointsSmall = [[1,4]; [3,5]; [2,3]; [1,1]; [3,2]; [5,1]; [4,3]; [2,2]; [3,4]; [4,6]; [2,5]; [3,7]; [1,8]; [2,6]; [1,4]]
 pointsBig    =[[1,4]; [2,6]; [1,8]; [3,7]; [5,8]; [4,6]; [2,7]; [3,5]; [5,6]; [4,4]; [5,2]; [3,1]; [1,2]; [3,3]; [1,4]]
 subplot(131)
 plot(pointsSmall(:,1),pointsSmall(:,2),'b', "linewidth", 5, pointsBig(:,1), pointsBig(:,2), 'r', "linewidth", 3)
 axis equal
 xlim([1,5])
 ylim([1,8])
 title("Knight tours of length 14")
 subplot(132)
 fill(pointsSmall(:,1),pointsSmall(:,2),'b')
 axis equal
 xlim([1,5])
 ylim([1,8])
 title("Area = 8")
 subplot(133)
 fill(pointsBig(:,1),pointsBig(:,2),'r')
 axis equal
 xlim([1,5])
 ylim([1,8])
 title("Area = 15")
 */

 println(immMap.maxBy(_._1)._1)
 printChessNotation(immMap.maxBy(_._1)._2)
 printWinner(immMap.maxBy(_._1)._2)
 
 val ma = scala.collection.mutable.Map[Int, List[(Int, Int)]]()
 
 val allSols = allTours.filter{ tour =>
  val area = calcArea(tour)
  ma += ((area,tour))
  val max = ma.maxBy(_._1)
	val min = ma.minBy(_._1)
  max._1 - area >= 7 ||  area - min._1 >= 7
  }.toList
  
  allSols.size
 
 printChessNotation(ma.minBy(_._1)._2)
 printChessNotation(ma.maxBy(_._1)._2)
 println(ma.minBy(_._1)._1)
 printWinner(ma.minBy(_._1)._2)
 println(ma.maxBy(_._1)._1)
  printWinner(ma.maxBy(_._1)._2)
  
 allSols.foreach(x=> {
  println("Winner size ", calcArea(x))
  printWinner(x)}
  )
 ma.foreach(println)
    
  

  
}