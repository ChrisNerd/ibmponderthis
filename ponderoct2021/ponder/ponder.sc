import java.io._
import sys.process._
object ponder {
//  val mazeAsciiString = "7e3593b53ec55e9e7a6ec759e9a66cb35ea9639c753c356633599336a5a97599556a9c6aa553cc6355a3da56aa693aaae3c9"
// "9182df2ec797b9c88df0af877be505daa6f6575a3cf4c5623"
 val mazeAsciiString = "65dd9ac3e53d7aaa7aac39ea399a57cc6aa9393ac5399399a"
                                                  //> mazeAsciiString  : String = 65dd9ac3e53d7aaa7aac39ea399a57cc6aa9393ac5399399
                                                  //| a

 val m = 7 //10 //7                               //> m  : Int = 7
 val numSteps = 3 //7 //3                         //> numSteps  : Int = 3
 val mazeAscii2DString = mazeAsciiString.grouped(m).toList
                                                  //> mazeAscii2DString  : List[String] = List(65dd9ac, 3e53d7a, aa7aac3, 9ea399a,
                                                  //|  57cc6aa, 9393ac5, 399399a)
 val mazeOriginal = mazeAscii2DString.map(_.split("").map(x => BigInt(x,16).toString(2).reverse.padTo(4, '0').reverse).map(x => x.toList).toList)
                                                  //> mazeOriginal  : List[List[List[Char]]] = List(List(List(0, 1, 1, 0), List(0,
                                                  //|  1, 0, 1), List(1, 1, 0, 1), List(1, 1, 0, 1), List(1, 0, 0, 1), List(1, 0, 
                                                  //| 1, 0), List(1, 1, 0, 0)), List(List(0, 0, 1, 1), List(1, 1, 1, 0), List(0, 1
                                                  //| , 0, 1), List(0, 0, 1, 1), List(1, 1, 0, 1), List(0, 1, 1, 1), List(1, 0, 1,
                                                  //|  0)), List(List(1, 0, 1, 0), List(1, 0, 1, 0), List(0, 1, 1, 1), List(1, 0, 
                                                  //| 1, 0), List(1, 0, 1, 0), List(1, 1, 0, 0), List(0, 0, 1, 1)), List(List(1, 0
                                                  //| , 0, 1), List(1, 1, 1, 0), List(1, 0, 1, 0), List(0, 0, 1, 1), List(1, 0, 0,
                                                  //|  1), List(1, 0, 0, 1), List(1, 0, 1, 0)), List(List(0, 1, 0, 1), List(0, 1, 
                                                  //| 1, 1), List(1, 1, 0, 0), List(1, 1, 0, 0), List(0, 1, 1, 0), List(1, 0, 1, 0
                                                  //| ), List(1, 0, 1, 0)), List(List(1, 0, 0, 1), List(0, 0, 1, 1), List(1, 0, 0,
                                                  //|  1), List(0, 0, 1, 1), List(1, 0, 1, 0), List(1, 1, 0, 0), List(0, 1, 0, 1))
                                                  //| , List(List(0, 0, 1, 1), List(1, 0, 0, 1), List(1, 0, 0, 1), List(0, 0, 1, 1
                                                  //| ), List(1, 0, 0, 1), List(1, 0, 0, 1), List(1, 0, 1, 0)))
  
 // Should probably clean this up!!!
 def neighbours(maze : List[List[(List[Char], Boolean)]])(p: (Int, Int)) : Set[(Int,Int)] = {
  val i = p._1
  val j = p._2
  val candidatesNeighbours = List(
  (0, -1),  // left
  (0,  1), // right
  (-1, 0), // up
  (1,  0)).map{ case(ioffset,joffset) => (i + ioffset, j + joffset) }
  
  //println("p + " + p)
  //println("cands " + candidatesNeighbours)
  
  
  candidatesNeighbours.zipWithIndex
  .filterNot{ case ((ioffset, joffset),index) => ioffset < 0 || ioffset >= m || joffset < 0 || joffset >= m }
  .filter{ case((ioffset,joffset),index) => index match {
   case 0 => maze(ioffset)(joffset)._1(1) == '1' && maze(i)(j)._1(3) == '1'
   case 1 => maze(ioffset)(joffset)._1(3) == '1' && maze(i)(j)._1(1) == '1'
   case 2 => maze(ioffset)(joffset)._1(2) == '1' && maze(i)(j)._1(0) == '1'
   case 3 => maze(ioffset)(joffset)._1(0) == '1' && maze(i)(j)._1(2) == '1'} }.map(_._1).toSet
 }                                                //> neighbours: (maze: List[List[(List[Char], Boolean)]])(p: (Int, Int))Set[(In
                                                  //| t, Int)]
 
 
 
 def shiftRow(r: Int, maze: List[List[ (List[Char], Boolean)]]): List[List[ (List[Char], Boolean)]] = {
  maze.updated(r, maze(r).last :: maze(r).take(m-1))
  // need to updated the visited, unless that's in the maze already List[List[ (List[Char], Boolean)]]
 }                                                //> shiftRow: (r: Int, maze: List[List[(List[Char], Boolean)]])List[List[(List[
                                                  //| Char], Boolean)]]

 val mazeOriginalWithVisited = mazeOriginal.map( r=> r.map( cell => (cell,false)))
                                                  //> mazeOriginalWithVisited  : List[List[(List[Char], Boolean)]] = List(List((L
                                                  //| ist(0, 1, 1, 0),false), (List(0, 1, 0, 1),false), (List(1, 1, 0, 1),false),
                                                  //|  (List(1, 1, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0),fals
                                                  //| e), (List(1, 1, 0, 0),false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1
                                                  //| , 0),false), (List(0, 1, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 1
                                                  //| , 0, 1),false), (List(0, 1, 1, 1),false), (List(1, 0, 1, 0),false)), List((
                                                  //| List(1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(0, 1, 1, 1),false)
                                                  //| , (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),fal
                                                  //| se), (List(0, 0, 1, 1),false)), List((List(1, 0, 0, 1),false), (List(1, 1, 
                                                  //| 1, 0),false), (List(1, 0, 1, 0),false), (List(0, 0, 1, 1),false), (List(1, 
                                                  //| 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0),false)), List(
                                                  //| (List(0, 1, 0, 1),false), (List(0, 1, 1, 1),false), (List(1, 1, 0, 0),false
                                                  //| ), (List(1, 1, 0, 0),false), (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),fa
                                                  //| lse), (List(1, 0, 1, 0),false)), List((List(1, 0, 0, 1),false), (List(0, 0,
                                                  //|  1, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (List(1,
                                                  //|  0, 1, 0),false), (List(1, 1, 0, 0),false), (List(0, 1, 0, 1),false)), List
                                                  //| ((List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),fals
                                                  //| e), (List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),f
                                                  //| alse), (List(1, 0, 1, 0),false)))
 val mazeOriginalWithVisitedInitialized = mazeOriginalWithVisited.updated(0, mazeOriginalWithVisited(0).updated(0,
 (mazeOriginalWithVisited(0)(0)._1, true)))       //> mazeOriginalWithVisitedInitialized  : List[List[(List[Char], Boolean)]] = L
                                                  //| ist(List((List(0, 1, 1, 0),true), (List(0, 1, 0, 1),false), (List(1, 1, 0, 
                                                  //| 1),false), (List(1, 1, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 
                                                  //| 1, 0),false), (List(1, 1, 0, 0),false)), List((List(0, 0, 1, 1),false), (Li
                                                  //| st(1, 1, 1, 0),false), (List(0, 1, 0, 1),false), (List(0, 0, 1, 1),false), 
                                                  //| (List(1, 1, 0, 1),false), (List(0, 1, 1, 1),false), (List(1, 0, 1, 0),false
                                                  //| )), List((List(1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(0, 1, 1,
                                                  //|  1),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 1,
                                                  //|  0, 0),false), (List(0, 0, 1, 1),false)), List((List(1, 0, 0, 1),false), (L
                                                  //| ist(1, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(0, 0, 1, 1),false),
                                                  //|  (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0),fals
                                                  //| e)), List((List(0, 1, 0, 1),false), (List(0, 1, 1, 1),false), (List(1, 1, 0
                                                  //| , 0),false), (List(1, 1, 0, 0),false), (List(0, 1, 1, 0),false), (List(1, 0
                                                  //| , 1, 0),false), (List(1, 0, 1, 0),false)), List((List(1, 0, 0, 1),false), (
                                                  //| List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false)
                                                  //| , (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(0, 1, 0, 1),fal
                                                  //| se)), List((List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 
                                                  //| 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (List(1, 
                                                  //| 0, 0, 1),false), (List(1, 0, 1, 0),false)))
 
 mazeOriginalWithVisitedInitialized(0)(0)         //> res0: (List[Char], Boolean) = (List(0, 1, 1, 0),true)
 mazeOriginalWithVisitedInitialized(0)(1)         //> res1: (List[Char], Boolean) = (List(0, 1, 0, 1),false)
 mazeOriginalWithVisitedInitialized(1)(0)         //> res2: (List[Char], Boolean) = (List(0, 0, 1, 1),false)
 
 
 neighbours(mazeOriginalWithVisitedInitialized)( 0, 0) // Should be Set((1,0))
                                                  //> res3: Set[(Int, Int)] = Set((0,1))
 neighbours(mazeOriginalWithVisitedInitialized)( 0, 1) // Should be Set((0,2))
                                                  //> res4: Set[(Int, Int)] = Set((0,0), (0,2))
 neighbours(mazeOriginalWithVisitedInitialized)( 0, 2) // Should be Set((0,1))
                                                  //> res5: Set[(Int, Int)] = Set((0,1), (0,3))
 neighbours(mazeOriginalWithVisitedInitialized)( 1, 0) // Should be Set((0,0),(1,1))
                                                  //> res6: Set[(Int, Int)] = Set((2,0))
 neighbours(mazeOriginalWithVisitedInitialized)( 2, 0) // Should be Set((2,1))
                                                  //> res7: Set[(Int, Int)] = Set((1,0), (3,0))
 
 neighbours(mazeOriginalWithVisitedInitialized)( 3, 3)
                                                  //> res8: Set[(Int, Int)] = Set((4,3))
 neighbours(mazeOriginalWithVisitedInitialized)( 4, 7)
                                                  //> res9: Set[(Int, Int)] = Set()
 neighbours(mazeOriginalWithVisitedInitialized)( 4, 8)
                                                  //> res10: Set[(Int, Int)] = Set()
 neighbours(mazeOriginalWithVisitedInitialized)( 8, 4)
                                                  //> res11: Set[(Int, Int)] = Set()


 shiftRow(0, mazeOriginalWithVisitedInitialized)  //> res12: List[List[(List[Char], Boolean)]] = List(List((List(1, 1, 0, 0),fals
                                                  //| e), (List(0, 1, 1, 0),true), (List(0, 1, 0, 1),false), (List(1, 1, 0, 1),fa
                                                  //| lse), (List(1, 1, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0)
                                                  //| ,false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1, 0),false), (List(0,
                                                  //|  1, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 1, 0, 1),false), (List
                                                  //| (0, 1, 1, 1),false), (List(1, 0, 1, 0),false)), List((List(1, 0, 1, 0),fals
                                                  //| e), (List(1, 0, 1, 0),false), (List(0, 1, 1, 1),false), (List(1, 0, 1, 0),f
                                                  //| alse), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(0, 0, 1, 1
                                                  //| ),false)), List((List(1, 0, 0, 1),false), (List(1, 1, 1, 0),false), (List(1
                                                  //| , 0, 1, 0),false), (List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (Lis
                                                  //| t(1, 0, 0, 1),false), (List(1, 0, 1, 0),false)), List((List(0, 1, 0, 1),fal
                                                  //| se), (List(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(1, 1, 0, 0),
                                                  //| false), (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 
                                                  //| 0),false)), List((List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (List(
                                                  //| 1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 0, 1, 0),false), (Li
                                                  //| st(1, 1, 0, 0),false), (List(0, 1, 0, 1),false)), List((List(0, 0, 1, 1),fa
                                                  //| lse), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1)
                                                  //| ,false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1,
                                                  //|  0),false)))
 def shiftColumn(c: Int, maze: List[List[ (List[Char], Boolean)]]): List[List[ (List[Char], Boolean)]] = {
  assert(c<10, "c is not less than 10")
  maze.zipWithIndex.map{ case (row,index) => row.updated(c, maze((index - 1 + m) % m)(c))}
 }                                                //> shiftColumn: (c: Int, maze: List[List[(List[Char], Boolean)]])List[List[(Li
                                                  //| st[Char], Boolean)]]
 shiftColumn(0, mazeOriginalWithVisitedInitialized)
                                                  //> res13: List[List[(List[Char], Boolean)]] = List(List((List(0, 0, 1, 1),fals
                                                  //| e), (List(0, 1, 0, 1),false), (List(1, 1, 0, 1),false), (List(1, 1, 0, 1),f
                                                  //| alse), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0
                                                  //| ),false)), List((List(0, 1, 1, 0),true), (List(1, 1, 1, 0),false), (List(0,
                                                  //|  1, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 1, 0, 1),false), (List
                                                  //| (0, 1, 1, 1),false), (List(1, 0, 1, 0),false)), List((List(0, 0, 1, 1),fals
                                                  //| e), (List(1, 0, 1, 0),false), (List(0, 1, 1, 1),false), (List(1, 0, 1, 0),f
                                                  //| alse), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(0, 0, 1, 1
                                                  //| ),false)), List((List(1, 0, 1, 0),false), (List(1, 1, 1, 0),false), (List(1
                                                  //| , 0, 1, 0),false), (List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (Lis
                                                  //| t(1, 0, 0, 1),false), (List(1, 0, 1, 0),false)), List((List(1, 0, 0, 1),fal
                                                  //| se), (List(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(1, 1, 0, 0),
                                                  //| false), (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 
                                                  //| 0),false)), List((List(0, 1, 0, 1),false), (List(0, 0, 1, 1),false), (List(
                                                  //| 1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 0, 1, 0),false), (Li
                                                  //| st(1, 1, 0, 0),false), (List(0, 1, 0, 1),false)), List((List(1, 0, 0, 1),fa
                                                  //| lse), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1)
                                                  //| ,false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1,
                                                  //|  0),false)))
 
  // Breadth first search, returns all points that are reachable from any point in Frontier.
 def bfs( maze: List[List[ (List[Char], Boolean)]] , marked: Set[(Int, Int)], frontier: Set[(Int, Int)]):  List[List[ (List[Char], Boolean)]] =
 {
   if (frontier.nonEmpty) {
    val newFrontier = frontier flatMap neighbours(maze)
    //println("non empty" + newFrontier)
    bfs(maze, marked ++ newFrontier ++ frontier, newFrontier -- marked -- frontier)
   }
  else {
  //println(marked)
    for {
     i <- (0 until m).toList
    } yield
    for {
     j <- (0 until m).toList
    } yield
    if (marked(i,j))
     (maze(i)(j)._1, true)
      else
     (maze(i)(j)._1, false)
  }
 }                                                //> bfs: (maze: List[List[(List[Char], Boolean)]], marked: Set[(Int, Int)], fro
                                                  //| ntier: Set[(Int, Int)])List[List[(List[Char], Boolean)]]
 
 
 // update the visited Booleans with all the reachable cells
 def walk(maze: List[List[ (List[Char], Boolean)]]): List[List[ (List[Char], Boolean)]] = {
  val v = for{ i <- 0 until m
  j <- 0 until m
  if (maze(i)(j)._2)
   } yield((i,j))
   bfs(maze, v.toSet, v.toSet)

 }                                                //> walk: (maze: List[List[(List[Char], Boolean)]])List[List[(List[Char], Boole
                                                  //| an)]]
 val v = for{ i <- 0 until m
  j <- 0 until m
  if (mazeOriginalWithVisitedInitialized(i)(j)._2)
   } yield((i,j))                                 //> v  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((0,0))
   v.toSet                                        //> res14: scala.collection.immutable.Set[(Int, Int)] = Set((0,0))
   
 val newFrontier = v.toSet flatMap neighbours(mazeOriginalWithVisitedInitialized)
                                                  //> newFrontier  : scala.collection.immutable.Set[(Int, Int)] = Set((0,1))
   
 
 
 //val step1 = shiftRow(0, walk(mazeOriginalWithVisitedInitialized))
 //val step3 = shiftColumn(3, walk(step1))
    /*
    // Brute force method
 val sols = for {
 move1RowOrCol <- Iterator(0 to 1)
 move1num <- 0 until 10
 move2RowOrCol <- 0 to 1
 move2num <- 0 until 10
 move3RowOrCol <- 0 to 1
 move3num <- 0 until 10
 move4RowOrCol <- 0 to 1
 move4num <- 0 until 10
 move5RowOrCol <- 0 to 1
 move5num <- 0 until 10
 move6RowOrCol <- 0 to 1
 move6num <- 0 until 10
 move7RowOrCol <- 0 to 1
 move7num <- 0 until 10
 val step1 = if (move1RowOrCol == 0)
  shiftRow(move1num, walk(mazeOriginalWithVisitedInitialized))
  else
  shiftColumn(move1num, walk(mazeOriginalWithVisitedInitialized))
 val step2 = if (move2RowOrCol == 0)
  shiftRow(move2num, walk(step1))
  else
 shiftColumn(move2num, walk(step1))
 val step3 = if (move3RowOrCol == 0)
  shiftRow(move3num, walk(step2))
  else
 shiftColumn(move3num, walk(step2))
 val step4 = if (move4RowOrCol == 0)
  shiftRow(move4num, walk(step3))
  else
 shiftColumn(move4num, walk(step3))
 val step5 = if (move5RowOrCol == 0)
  shiftRow(move5num, walk(step4))
  else
 shiftColumn(move5num, walk(step4))
 val step6 = if (move6RowOrCol == 0)
  shiftRow(move6num, walk(step5))
  else
 shiftColumn(move6num, walk(step5))
 val step7 = if (move7RowOrCol == 0)
  shiftRow(move7num, walk(step6))
  else
 shiftColumn(move7num, walk(step6))
 if (step7(9)(9)._2)
 } yield (move1RowOrCol,move1num,
 move2RowOrCol,move2num,
 move3RowOrCol,move3num,
 move4RowOrCol,move4num,
 move5RowOrCol,move5num,
 move6RowOrCol,move6num,
 move7RowOrCol,move7num)
 
 sols.foreach(println)
 */
 
 def neighbourOfInput( input: List[Int]) : List[Int] = {
  val i = scala.util.Random.nextInt(2*numSteps)
  if (i % 2 == 0) // then we'll toggle a row to a column
   input.updated(i, 1 - input(i))
  else
   if (scala.util.Random.nextBoolean())
    input.updated(i, (input(i) + 1) % m)
   else
    input.updated(i, (input(i) - 1 + m) % m)
  }                                               //> neighbourOfInput: (input: List[Int])List[Int]
  
 def shiftRowOrColumn( maze: List[List[(List[Char], Boolean)]], input: List[Int]) = {
  if(m==7)  // The bonus question goes shift-move-shift-move
  // while the 7x7 grid goes move-shift-move-shift
   if (input(0) == 0)
    shiftRow(input(1), walk(maze))
   else
    shiftColumn(input(1), walk(maze))
  else
   if (input(0) == 0)
    walk(shiftRow(input(1), maze))
   else
    walk(shiftColumn(input(1), maze))
 }                                                //> shiftRowOrColumn: (maze: List[List[(List[Char], Boolean)]], input: List[Int
                                                  //| ])List[List[(List[Char], Boolean)]]
 
 def score( input: List[Int] ): Int = {
 val finalmaze = input.grouped(2).foldLeft(mazeOriginalWithVisitedInitialized)(shiftRowOrColumn)
 val furthestReach = (for{
  i <- 0 until m
  j <- 0 until m
  // step7!!!
  if (finalmaze(i)(j)._2)
  } yield i+j ).max
 
  2*(m-1) - furthestReach
 }                                                //> score: (input: List[Int])Int
 val (initialTemperature, finalTemperature, coolingRate) = (100.0, 0.1, 0.0005)
                                                  //> initialTemperature  : Double = 100.0
                                                  //| finalTemperature  : Double = 0.1
                                                  //| coolingRate  : Double = 5.0E-4
 val scoreToBeat = 0                              //> scoreToBeat  : Int = 0
 def simulatedAnnealing( best: List[Int], temp:Double): List[Int] = {
  //println("Temp " + temp)
  if (temp > finalTemperature) {
   val currentEnergy = score(best)
   val neighbour = neighbourOfInput( best )
   val neighbourEnergy = score(neighbour)
   if (neighbourEnergy <= 2)
   {
//    println("Energy " + neighbourEnergy )
//    println(neighbour)
   }
   
   if (neighbourEnergy <= scoreToBeat)
   {
    println("Solution!!!")
    println(neighbour)
    neighbour
   }
   else
   {   // Decide if we should accept the neighbour
//    println("neighbourEnergy " + neighbourEnergy + " currentEnergy " + currentEnergy + " temp " + temp)
    val acceptanceProbability = math.exp(-(neighbourEnergy - currentEnergy)/temp)
 //   println("acceptance probability " + acceptanceProbability)
    val accept = (acceptanceProbability > math.random)
    simulatedAnnealing( if (accept) {
     // println("\nScore: " + neighbourEnergy, " Temperature: "+ temp)
     neighbour
     } else best, (1-coolingRate)*temp)
   }
  } else best
 }                                                //> simulatedAnnealing: (best: List[Int], temp: Double)List[Int]
 def printSVGBoard(maze: List[List[(List[Char], Boolean)]], number: Int, N: Int) = {
  val pw = new PrintWriter(new File("%07d".format(number) + ".svg" ))
  pw.write("<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"-0.5 -0.5 11 11\" height=\"" + 800 + "\" width=\"" + 800 + "\" >\n")
  pw.write("<rect x=\"-1\" y=\"-1\" width=\"11\" height=\"11\" fill=\"white\" />")
  (0 until N).foreach{ y =>
   (0 until N).foreach{ x =>
    if ( maze(y)(x)._2 )
     pw.write("<circle cx=\"" + x + "\" cy=\"" + y + "\" r=\"0.4\" stroke=\"black\" stroke-width=\"0.003\" fill=\"green\" opacity=\".5\" />\n")
    maze(y)(x)._1.zipWithIndex.foreach{ case (c,i) => if (c == '1') i match {
      case 0 => pw.write("<line x1=\""+ x + "\" y1=\"" + y + "\" x2=\"" + x +      "\" y2=\"" + (y-.4) + "\" stroke=\"#000\" stroke-width=\"0.008\" />\n")
      case 1 => pw.write("<line x1=\""+ x + "\" y1=\"" + y + "\" x2=\"" + (x+.4) + "\" y2=\"" + y      + "\" stroke=\"#000\" stroke-width=\"0.008\" />\n")
      case 2 => pw.write("<line x1=\""+ x + "\" y1=\"" + y + "\" x2=\"" + x +      "\" y2=\"" + (y+.4) + "\" stroke=\"#000\" stroke-width=\"0.008\" />\n")
      case 3 => pw.write("<line x1=\""+ x + "\" y1=\"" + y + "\" x2=\"" + (x-.4) + "\" y2=\"" + y      + "\" stroke=\"#000\" stroke-width=\"0.008\" />\n")
     }
    }
   }
  }
  pw.write("\n")
  pw.write("</svg>")
  pw.close
 }                                                //> printSVGBoard: (maze: List[List[(List[Char], Boolean)]], number: Int, N: In
                                                  //| t)Unit
 printSVGBoard(mazeOriginalWithVisitedInitialized, 0, m)
 
// val getsScore1 = List(1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 0, 8, 0, 9)
 val getsScore1 = List(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 8, 0, 9)
                                                  //> getsScore1  : List[Int] = List(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 8, 0, 9)
 
 
 val mazeStep1 = walk(mazeOriginalWithVisitedInitialized)
                                                  //> mazeStep1  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0
                                                  //| ),true), (List(0, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(1, 1, 0, 1
                                                  //| ),true), (List(1, 0, 0, 1),true), (List(1, 0, 1, 0),false), (List(1, 1, 0, 
                                                  //| 0),false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1, 0),false), (List(
                                                  //| 0, 1, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 1, 0, 1),false), (Li
                                                  //| st(0, 1, 1, 1),false), (List(1, 0, 1, 0),false)), List((List(1, 0, 1, 0),fa
                                                  //| lse), (List(1, 0, 1, 0),false), (List(0, 1, 1, 1),false), (List(1, 0, 1, 0)
                                                  //| ,false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(0, 0, 1,
                                                  //|  1),false)), List((List(1, 0, 0, 1),false), (List(1, 1, 1, 0),false), (List
                                                  //| (1, 0, 1, 0),false), (List(0, 0, 1, 1),false), (List(1, 0, 0, 1),false), (L
                                                  //| ist(1, 0, 0, 1),false), (List(1, 0, 1, 0),false)), List((List(0, 1, 0, 1),f
                                                  //| alse), (List(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(1, 1, 0, 0
                                                  //| ),false), (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1
                                                  //| , 0),false)), List((List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (Lis
                                                  //| t(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 0, 1, 0),false), (
                                                  //| List(1, 1, 0, 0),false), (List(0, 1, 0, 1),false)), List((List(0, 0, 1, 1),
                                                  //| false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 
                                                  //| 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 
                                                  //| 1, 0),false)))
 val mazeStep2 = shiftColumn(3, mazeStep1)        //> mazeStep2  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0
                                                  //| ),true), (List(0, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0, 0, 1, 1
                                                  //| ),false), (List(1, 0, 0, 1),true), (List(1, 0, 1, 0),false), (List(1, 1, 0,
                                                  //|  0),false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1, 0),false), (List
                                                  //| (0, 1, 0, 1),false), (List(1, 1, 0, 1),true), (List(1, 1, 0, 1),false), (Li
                                                  //| st(0, 1, 1, 1),false), (List(1, 0, 1, 0),false)), List((List(1, 0, 1, 0),fa
                                                  //| lse), (List(1, 0, 1, 0),false), (List(0, 1, 1, 1),false), (List(0, 0, 1, 1)
                                                  //| ,false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(0, 0, 1,
                                                  //|  1),false)), List((List(1, 0, 0, 1),false), (List(1, 1, 1, 0),false), (List
                                                  //| (1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),false), (L
                                                  //| ist(1, 0, 0, 1),false), (List(1, 0, 1, 0),false)), List((List(0, 1, 0, 1),f
                                                  //| alse), (List(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(0, 0, 1, 1
                                                  //| ),false), (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1
                                                  //| , 0),false)), List((List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (Lis
                                                  //| t(1, 0, 0, 1),false), (List(1, 1, 0, 0),false), (List(1, 0, 1, 0),false), (
                                                  //| List(1, 1, 0, 0),false), (List(0, 1, 0, 1),false)), List((List(0, 0, 1, 1),
                                                  //| false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 
                                                  //| 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 
                                                  //| 1, 0),false)))
 val mazeStep3 = walk(mazeStep2)                  //> mazeStep3  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0
                                                  //| ),true), (List(0, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0, 0, 1, 1
                                                  //| ),true), (List(1, 0, 0, 1),true), (List(1, 0, 1, 0),false), (List(1, 1, 0, 
                                                  //| 0),false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1, 0),true), (List(0
                                                  //| , 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0
                                                  //| , 1, 1, 1),true), (List(1, 0, 1, 0),false)), List((List(1, 0, 1, 0),false),
                                                  //|  (List(1, 0, 1, 0),true), (List(0, 1, 1, 1),false), (List(0, 0, 1, 1),false
                                                  //| ), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),true), (List(0, 0, 1, 1),tru
                                                  //| e)), List((List(1, 0, 0, 1),false), (List(1, 1, 1, 0),true), (List(1, 0, 1,
                                                  //|  0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),false), (List(1, 0,
                                                  //|  0, 1),false), (List(1, 0, 1, 0),true)), List((List(0, 1, 0, 1),false), (Li
                                                  //| st(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(0, 0, 1, 1),false), 
                                                  //| (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),true)
                                                  //| ), List((List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (List(1, 0, 0, 
                                                  //| 1),false), (List(1, 1, 0, 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 
                                                  //| 0, 0),false), (List(0, 1, 0, 1),false)), List((List(0, 0, 1, 1),false), (Li
                                                  //| st(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), 
                                                  //| (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0),false
                                                  //| )))
 val mazeStep4 = shiftRow(5, mazeStep3)           //> mazeStep4  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0
                                                  //| ),true), (List(0, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0, 0, 1, 1
                                                  //| ),true), (List(1, 0, 0, 1),true), (List(1, 0, 1, 0),false), (List(1, 1, 0, 
                                                  //| 0),false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1, 0),true), (List(0
                                                  //| , 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0
                                                  //| , 1, 1, 1),true), (List(1, 0, 1, 0),false)), List((List(1, 0, 1, 0),false),
                                                  //|  (List(1, 0, 1, 0),true), (List(0, 1, 1, 1),false), (List(0, 0, 1, 1),false
                                                  //| ), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),true), (List(0, 0, 1, 1),tru
                                                  //| e)), List((List(1, 0, 0, 1),false), (List(1, 1, 1, 0),true), (List(1, 0, 1,
                                                  //|  0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),false), (List(1, 0,
                                                  //|  0, 1),false), (List(1, 0, 1, 0),true)), List((List(0, 1, 0, 1),false), (Li
                                                  //| st(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(0, 0, 1, 1),false), 
                                                  //| (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),true)
                                                  //| ), List((List(0, 1, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 
                                                  //| 1),false), (List(1, 0, 0, 1),false), (List(1, 1, 0, 0),false), (List(1, 0, 
                                                  //| 1, 0),false), (List(1, 1, 0, 0),false)), List((List(0, 0, 1, 1),false), (Li
                                                  //| st(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), 
                                                  //| (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0),false
                                                  //| )))
 val mazeStep5 = walk(mazeStep4)                  //> mazeStep5  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0
                                                  //| ),true), (List(0, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0, 0, 1, 1
                                                  //| ),true), (List(1, 0, 0, 1),true), (List(1, 0, 1, 0),false), (List(1, 1, 0, 
                                                  //| 0),false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1, 0),true), (List(0
                                                  //| , 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0
                                                  //| , 1, 1, 1),true), (List(1, 0, 1, 0),false)), List((List(1, 0, 1, 0),false),
                                                  //|  (List(1, 0, 1, 0),true), (List(0, 1, 1, 1),false), (List(0, 0, 1, 1),false
                                                  //| ), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),true), (List(0, 0, 1, 1),tru
                                                  //| e)), List((List(1, 0, 0, 1),false), (List(1, 1, 1, 0),true), (List(1, 0, 1,
                                                  //|  0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),false), (List(1, 0,
                                                  //|  0, 1),false), (List(1, 0, 1, 0),true)), List((List(0, 1, 0, 1),false), (Li
                                                  //| st(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(0, 0, 1, 1),false), 
                                                  //| (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),true)
                                                  //| ), List((List(0, 1, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 
                                                  //| 1),false), (List(1, 0, 0, 1),false), (List(1, 1, 0, 0),false), (List(1, 0, 
                                                  //| 1, 0),false), (List(1, 1, 0, 0),true)), List((List(0, 0, 1, 1),false), (Lis
                                                  //| t(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), (
                                                  //| List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 0, 1, 0),false)
                                                  //| ))
 val mazeStep6 = shiftColumn(6, mazeStep5)        //> mazeStep6  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0
                                                  //| ),true), (List(0, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0, 0, 1, 1
                                                  //| ),true), (List(1, 0, 0, 1),true), (List(1, 0, 1, 0),false), (List(1, 0, 1, 
                                                  //| 0),false)), List((List(0, 0, 1, 1),false), (List(1, 1, 1, 0),true), (List(0
                                                  //| , 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(1, 1, 0, 1),true), (List(0
                                                  //| , 1, 1, 1),true), (List(1, 1, 0, 0),false)), List((List(1, 0, 1, 0),false),
                                                  //|  (List(1, 0, 1, 0),true), (List(0, 1, 1, 1),false), (List(0, 0, 1, 1),false
                                                  //| ), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),true), (List(1, 0, 1, 0),fal
                                                  //| se)), List((List(1, 0, 0, 1),false), (List(1, 1, 1, 0),true), (List(1, 0, 1
                                                  //| , 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),false), (List(1, 0
                                                  //| , 0, 1),false), (List(0, 0, 1, 1),true)), List((List(0, 1, 0, 1),false), (L
                                                  //| ist(0, 1, 1, 1),false), (List(1, 1, 0, 0),false), (List(0, 0, 1, 1),false),
                                                  //|  (List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),true
                                                  //| )), List((List(0, 1, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1,
                                                  //|  1),false), (List(1, 0, 0, 1),false), (List(1, 1, 0, 0),false), (List(1, 0,
                                                  //|  1, 0),false), (List(1, 0, 1, 0),true)), List((List(0, 0, 1, 1),false), (Li
                                                  //| st(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(0, 0, 1, 1),false), 
                                                  //| (List(1, 0, 0, 1),false), (List(1, 0, 0, 1),false), (List(1, 1, 0, 0),true)
                                                  //| ))
 // at most 6 turns.
 /*
 val mazeStep7 = walk(mazeStep6)
 val mazeStep8 = shiftRow(5, mazeStep7)
 val mazeStep9 = walk(mazeStep8)
 val mazeStep10 = shiftRow(2, mazeStep9)
 val mazeStep11 = walk(mazeStep10)
 val mazeStep12 = shiftColumn(8, mazeStep11)
 val mazeStep13 = walk(mazeStep12)
 val mazeStep14 = shiftColumn(8, mazeStep13)
 val mazeStep15 = walk(mazeStep14)*/
 printSVGBoard(mazeStep1, 1, m)
 printSVGBoard(mazeStep2, 2, m)
 printSVGBoard(mazeStep3, 3, m)
 printSVGBoard(mazeStep4, 4, m)
 printSVGBoard(mazeStep5, 5, m)
 printSVGBoard(mazeStep6, 6, m)
 /*
 printSVGBoard(mazeStep7, 7, m)
 printSVGBoard(mazeStep8, 8, m)
 printSVGBoard(mazeStep9, 9, m)
 printSVGBoard(mazeStep10, 10, m)
 printSVGBoard(mazeStep11, 11, m)
 printSVGBoard(mazeStep12, 12, m)
 printSVGBoard(mazeStep13, 13, m)
 printSVGBoard(mazeStep14, 14, m)
 printSVGBoard(mazeStep15, 15, m)
 */
 
 
  
 List.fill(numSteps * 2)(0)                       //> res15: List[Int] = List(0, 0, 0, 0, 0, 0)
 val bools = List.fill(numSteps)(scala.util.Random.nextInt(2))
                                                  //> bools  : List[Int] = List(1, 1, 1)
 val nums = List.fill(numSteps)(scala.util.Random.nextInt(10))
                                                  //> nums  : List[Int] = List(1, 5, 1)
 
 val both =  bools.zip(nums)                      //> both  : List[(Int, Int)] = List((1,1), (1,5), (1,1))
 val botha = List(bools,nums).transpose.flatten   //> botha  : List[Int] = List(1, 1, 1, 5, 1, 1)
 
 score( List.fill(numSteps *2)(0))                //> res16: Int = 6
 
 simulatedAnnealing( List.fill(numSteps*2)(0), initialTemperature)
                                                  //> Solution!!!
                                                  //| List(1, 3, 1, 6, 1, 6)
                                                  //| res17: List[Int] = List(1, 3, 1, 6, 1, 6)
 
 
 val sol = Iterator.from(0).map( trial =>  {
  val bools = List.fill(numSteps)(scala.util.Random.nextInt(2))
  val nums = List.fill(numSteps)(scala.util.Random.nextInt(m))
 
  val both =  bools.zip(nums)
  val botha = List(bools,nums).transpose.flatten
 
  val simResult = simulatedAnnealing( botha, initialTemperature)
  val simScore = score(simResult)
  if (trial % 100  == 0)
  {
   println("Trial " + trial)
   println(simScore)
  }
  (simResult, simScore)
  }).find( _._2 == 0).get
  
 val bonusSol = List(1, 0, 0, 1, 0, 2, 0, 5, 0, 2, 1, 8, 1, 8)
 // C0, (1,3), R1, (2,5), R2, (1,7), R5, (2,7), R2, (3,8), C8, (7,7), C8, (9,9)
 
                                                   
 val shortSol = List(1, 3, 0, 5, 1, 6)
 // (0,3), C3, (4,6), R5, (5,6), C6
 val printmazeList = sol._1.grouped(2).scanLeft(mazeOriginalWithVisitedInitialized)(shiftRowOrColumn)
 printmazeList.zipWithIndex.foreach{ x => printSVGBoard(x._1, x._2, m)}
 
 
 println("done")
 
}