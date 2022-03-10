import java.io._
import sys.process._
object ponder {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
// val mazeAsciiString = "593eaacd395ac6eac5a556aa53e3d539bac799c65a5a33da69663a3a33c635a635c3535c95696aa67b3cd3c576993576559db7aaa3655ac5753a933b593eabeca599965ac5a363a3c3d6acaa37cce3c9ecc3cb6397b99b66aa6e37ac9c55caa3c5937d5a3aa5ca3a9a5bcac3bd5a96c3b"
/*
A bonus "*" will be given for solving the 10x10 maze

In at most 20 moves *without moving the mouse* except in the last move.
*/
 val mazeAsciiString = "63aaac95c57baca9eadcc6c575ed9a5c57eaa96395975533c65c66a95c979566abae9ac5bbc7b7a6ec9e3eab563659c5737a"
                                                  //> mazeAsciiString  : String = 63aaac95c57baca9eadcc6c575ed9a5c57eaa96395975533
                                                  //| c65c66a95c979566abae9ac5bbc7b7a6ec9e3eab563659c5737a
 // Given the 15x15 maze
  val m = 10 // 15 //10 //7                       //> m  : Int = 10
 // Find a sequence of moves for the mouse and row/column slides such that the mouse reaches the exit in at most 50 slides.
 
 val numSteps = 20 //7 //3                        //> numSteps  : Int = 20
 val mazeAscii2DString = mazeAsciiString.grouped(m).toList
                                                  //> mazeAscii2DString  : List[String] = List(63aaac95c5, 7baca9eadc, c6c575ed9a,
                                                  //|  5c57eaa963, 95975533c6, 5c66a95c97, 9566abae9a, c5bbc7b7a6, ec9e3eab56, 365
                                                  //| 9c5737a)
 val mazeOriginal = mazeAscii2DString.map(_.split("").map(x => BigInt(x,16).toString(2).reverse.padTo(4, '0').reverse).map(x => x.toList).toList)
                                                  //> mazeOriginal  : List[List[List[Char]]] = List(List(List(0, 1, 1, 0), List(0
                                                  //| , 0, 1, 1), List(1, 0, 1, 0), List(1, 0, 1, 0), List(1, 0, 1, 0), List(1, 1
                                                  //| , 0, 0), List(1, 0, 0, 1), List(0, 1, 0, 1), List(1, 1, 0, 0), List(0, 1, 0
                                                  //| , 1)), List(List(0, 1, 1, 1), List(1, 0, 1, 1), List(1, 0, 1, 0), List(1, 1
                                                  //| , 0, 0), List(1, 0, 1, 0), List(1, 0, 0, 1), List(1, 1, 1, 0), List(1, 0, 1
                                                  //| , 0), List(1, 1, 0, 1), List(1, 1, 0, 0)), List(List(1, 1, 0, 0), List(0, 1
                                                  //| , 1, 0), List(1, 1, 0, 0), List(0, 1, 0, 1), List(0, 1, 1, 1), List(0, 1, 0
                                                  //| , 1), List(1, 1, 1, 0), List(1, 1, 0, 1), List(1, 0, 0, 1), List(1, 0, 1, 0
                                                  //| )), List(List(0, 1, 0, 1), List(1, 1, 0, 0), List(0, 1, 0, 1), List(0, 1, 1
                                                  //| , 1), List(1, 1, 1, 0), List(1, 0, 1, 0), List(1, 0, 1, 0), List(1, 0, 0, 1
                                                  //| ), List(0, 1, 1, 0), List(0, 0, 1, 1)), List(List(1, 0, 0, 1), List(0, 1, 0
                                                  //| , 1), List(1, 0, 0, 1), List(0, 1, 1, 1), List(0, 1, 0, 1), List(0, 1, 0, 1
                                                  //| ), List(0, 0, 1, 1), Li
                                                  //| Output exceeds cutoff limit.
 val mazeOriginalWithVisited = mazeOriginal.map( r=> r.map( cell => (cell,false)))
                                                  //> mazeOriginalWithVisited  : List[List[(List[Char], Boolean)]] = List(List((L
                                                  //| ist(0, 1, 1, 0),false), (List(0, 0, 1, 1),false), (List(1, 0, 1, 0),false),
                                                  //|  (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),fals
                                                  //| e), (List(1, 0, 0, 1),false), (List(0, 1, 0, 1),false), (List(1, 1, 0, 0),f
                                                  //| alse), (List(0, 1, 0, 1),false)), List((List(0, 1, 1, 1),false), (List(1, 0
                                                  //| , 1, 1),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(1
                                                  //| , 0, 1, 0),false), (List(1, 0, 0, 1),false), (List(1, 1, 1, 0),false), (Lis
                                                  //| t(1, 0, 1, 0),false), (List(1, 1, 0, 1),false), (List(1, 1, 0, 0),false)), 
                                                  //| List((List(1, 1, 0, 0),false), (List(0, 1, 1, 0),false), (List(1, 1, 0, 0),
                                                  //| false), (List(0, 1, 0, 1),false), (List(0, 1, 1, 1),false), (List(0, 1, 0, 
                                                  //| 1),false), (List(1, 1, 1, 0),false), (List(1, 1, 0, 1),false), (List(1, 0, 
                                                  //| 0, 1),false), (List(1, 0, 1, 0),false)), List((List(0, 1, 0, 1),false), (Li
                                                  //| st(1, 1, 0, 0),false), 
                                                  //| Output exceeds cutoff limit.
 val mazeOriginalWithVisitedInitialized = mazeOriginalWithVisited.updated(0, mazeOriginalWithVisited(0).updated(0,
 (mazeOriginalWithVisited(0)(0)._1, true)))       //> mazeOriginalWithVisitedInitialized  : List[List[(List[Char], Boolean)]] = L
                                                  //| ist(List((List(0, 1, 1, 0),true), (List(0, 0, 1, 1),false), (List(1, 0, 1, 
                                                  //| 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 
                                                  //| 0, 0),false), (List(1, 0, 0, 1),false), (List(0, 1, 0, 1),false), (List(1, 
                                                  //| 1, 0, 0),false), (List(0, 1, 0, 1),false)), List((List(0, 1, 1, 1),false), 
                                                  //| (List(1, 0, 1, 1),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false
                                                  //| ), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),false), (List(1, 1, 1, 0),fa
                                                  //| lse), (List(1, 0, 1, 0),false), (List(1, 1, 0, 1),false), (List(1, 1, 0, 0)
                                                  //| ,false)), List((List(1, 1, 0, 0),false), (List(0, 1, 1, 0),false), (List(1,
                                                  //|  1, 0, 0),false), (List(0, 1, 0, 1),false), (List(0, 1, 1, 1),false), (List
                                                  //| (0, 1, 0, 1),false), (List(1, 1, 1, 0),false), (List(1, 1, 0, 1),false), (L
                                                  //| ist(1, 0, 0, 1),false), (List(1, 0, 1, 0),false)), List((List(0, 1, 0, 1),f
                                                  //| alse), (List(1, 1, 0, 0
                                                  //| Output exceeds cutoff limit.
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
 // Need to set the visited to false for the affected cells
 def shiftRight(r: Int, maze: List[List[ (List[Char], Boolean)]]): List[List[ (List[Char], Boolean)]] = {
  maze.updated(r, maze(r).last :: maze(r).take(m-1))
  // need to updated the visited, unless that's in the maze already List[List[ (List[Char], Boolean)]]
 }                                                //> shiftRight: (r: Int, maze: List[List[(List[Char], Boolean)]])List[List[(Lis
                                                  //| t[Char], Boolean)]]
 def shiftLeft(r: Int, maze: List[List[ (List[Char], Boolean)]]): List[List[ (List[Char], Boolean)]] = {
  maze.updated(r, maze(r).drop(1) :+ maze(r).head )
  // need to updated the visited, unless that's in the maze already List[List[ (List[Char], Boolean)]]
 }                                                //> shiftLeft: (r: Int, maze: List[List[(List[Char], Boolean)]])List[List[(List
                                                  //| [Char], Boolean)]]

 def shiftDown(c: Int, maze: List[List[ (List[Char], Boolean)]]): List[List[ (List[Char], Boolean)]] = {
  assert(c<15, "c is not less than 15")
  maze.zipWithIndex.map{ case (row,index) => row.updated(c, maze((index - 1 + m) % m)(c))}
 }                                                //> shiftDown: (c: Int, maze: List[List[(List[Char], Boolean)]])List[List[(List
                                                  //| [Char], Boolean)]]
 
 def shiftUp(c: Int, maze: List[List[ (List[Char], Boolean)]]): List[List[ (List[Char], Boolean)]] = {
  assert(c<15, "c is not less than 15")
  maze.zipWithIndex.map{ case (row,index) => row.updated(c, maze((index + 1 + m) % m)(c))}
 }                                                //> shiftUp: (c: Int, maze: List[List[(List[Char], Boolean)]])List[List[(List[C
                                                  //| har], Boolean)]]
 
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
 
  def neighbourOfInput( input: List[Int]) : List[Int] = {
  val i = scala.util.Random.nextInt(2*numSteps)
  if (i % 2 == 0) // then we'll toggle a row to a column
   if (scala.util.Random.nextBoolean())
    input.updated(i, (input(i) + 1) % 4)
   else
   input.updated(i, (input(i) -1 + 4) % 4)
   
   
  else
   if (scala.util.Random.nextBoolean())
    input.updated(i, (input(i) + 1) % m)
   else
    input.updated(i, (input(i) - 1 + m) % m)
  }                                               //> neighbourOfInput: (input: List[Int])List[Int]
  
 def shiftRowOrColumn( maze: List[List[(List[Char], Boolean)]], input: List[Int]) = {
  
  //if(m==7)  // The bonus question goes shift-move-shift-move
  // while the 7x7 grid goes move-shift-move-shift
/*   val outputMaze = if (input(0) == 0)
    shiftLeft(input(1), walk(maze))
   else if (input(0) == 1)
    shiftRight(input(1), walk(maze))
  else if (input(0) == 2)
   shiftUp(input(1), walk(maze))
  else //if (input(0) == 3)
   shiftDown(input(1), walk(maze))
  */
  
  val outputMaze = if (input(0) == 0)
    shiftLeft(input(1), maze)
   else if (input(0) == 1)
    shiftRight(input(1), maze)
  else if (input(0) == 2)
   shiftUp(input(1), maze)
  else //if (input(0) == 3)
   shiftDown(input(1), maze)
   
   // Blank out the row or column that was shifted
   
  val v = (for{ i <- 0 until m
  j <- 0 until m
  if (outputMaze(i)(j)._2)
   } yield((i,j))).toSet
   
   val vBlanked = if (input(0) == 0 || input(0) == 1)
    v.filterNot(r => r._1 == input(1))
    else
    v.filterNot(r => r._2 == input(1))
      
  (0 until m).toList.map( i =>
   (0 until m).toList.map(j => (outputMaze(i)(j)._1, vBlanked(i,j))
  ))
   
    /*
   if (input(0) == 0)
    walk(shiftRow(input(1), maze))
   else
    walk(shiftColumn(input(1), maze))*/
 }                                                //> shiftRowOrColumn: (maze: List[List[(List[Char], Boolean)]], input: List[Int
                                                  //| ])List[List[(List[Char], Boolean)]]
 shiftRowOrColumn(mazeOriginalWithVisitedInitialized, List(0,1))
                                                  //> res0: List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0),true)
                                                  //| , (List(0, 0, 1, 1),false), (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),fal
                                                  //| se), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(1, 0, 0, 1),
                                                  //| false), (List(0, 1, 0, 1),false), (List(1, 1, 0, 0),false), (List(0, 1, 0, 
                                                  //| 1),false)), List((List(1, 0, 1, 1),false), (List(1, 0, 1, 0),false), (List(
                                                  //| 1, 1, 0, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),false), (Li
                                                  //| st(1, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 1),false), 
                                                  //| (List(1, 1, 0, 0),false), (List(0, 1, 1, 1),false)), List((List(1, 1, 0, 0)
                                                  //| ,false), (List(0, 1, 1, 0),false), (List(1, 1, 0, 0),false), (List(0, 1, 0,
                                                  //|  1),false), (List(0, 1, 1, 1),false), (List(0, 1, 0, 1),false), (List(1, 1,
                                                  //|  1, 0),false), (List(1, 1, 0, 1),false), (List(1, 0, 0, 1),false), (List(1,
                                                  //|  0, 1, 0),false)), List((List(0, 1, 0, 1),false), (List(1, 1, 0, 0),false),
                                                  //|  (List(0, 1, 0, 1),fals
                                                  //| Output exceeds cutoff limit.
 
 def score( input: List[Int] ): Int = {
 val finalmaze = walk(input.grouped(2).foldLeft(mazeOriginalWithVisitedInitialized)(shiftRowOrColumn))
  val f = for{
  i <- 0 until m
  j <- 0 until m
  // step7!!!
  if (finalmaze(i)(j)._2)
  } yield i+j
 
  val furthestReach = if (f.isEmpty) 0 else f.max
 
  2*(m-1) - furthestReach
 }                                                //> score: (input: List[Int])Int
 val (initialTemperature, finalTemperature, coolingRate) = (100.0, 0.1, 0.0005)
                                                  //> initialTemperature  : Double = 100.0
                                                  //| finalTemperature  : Double = 0.1
                                                  //| coolingRate  : Double = 5.0E-4
 
 
  
  
  val scoreToBeat = 0                             //> scoreToBeat  : Int = 0
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
  pw.write("<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"-0.5 -0.5 16 16\" height=\"" + 800 + "\" width=\"" + 800 + "\" >\n")
  pw.write("<rect x=\"-1\" y=\"-1\" width=\"16\" height=\"16\" fill=\"white\" />")
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
 printSVGBoard(shiftRowOrColumn(mazeOriginalWithVisitedInitialized, List(0,1)), 1, m)
 printSVGBoard(shiftRowOrColumn(mazeOriginalWithVisitedInitialized, List(0,0)), 2, m)
 printSVGBoard(shiftRowOrColumn(mazeOriginalWithVisitedInitialized, List(1,3)), 3, m)
 printSVGBoard(shiftRowOrColumn(mazeOriginalWithVisitedInitialized, List(2,5)), 4, m)
 printSVGBoard(shiftRowOrColumn(mazeOriginalWithVisitedInitialized, List(3,6)), 5, m)
 val finalmaze = List(0,1,3,2,2,9).grouped(2).foldLeft(mazeOriginalWithVisitedInitialized)(shiftRowOrColumn)
                                                  //> finalmaze  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 0
                                                  //| ),true), (List(0, 0, 1, 1),false), (List(0, 1, 0, 1),false), (List(1, 0, 1,
                                                  //|  0),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(1, 0,
                                                  //|  0, 1),false), (List(0, 1, 0, 1),false), (List(1, 1, 0, 0),false), (List(0,
                                                  //|  1, 1, 1),false)), List((List(1, 0, 1, 1),false), (List(1, 0, 1, 0),false),
                                                  //|  (List(1, 0, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),fals
                                                  //| e), (List(1, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 1),f
                                                  //| alse), (List(1, 1, 0, 0),false), (List(1, 0, 1, 0),false)), List((List(1, 1
                                                  //| , 0, 0),false), (List(0, 1, 1, 0),false), (List(1, 1, 0, 0),false), (List(0
                                                  //| , 1, 0, 1),false), (List(0, 1, 1, 1),false), (List(0, 1, 0, 1),false), (Lis
                                                  //| t(1, 1, 1, 0),false), (List(1, 1, 0, 1),false), (List(1, 0, 0, 1),false), (
                                                  //| List(0, 0, 1, 1),false)), List((List(0, 1, 0, 1),false), (List(1, 1, 0, 0),
                                                  //| false), (List(1, 1, 0, 
                                                  //| Output exceeds cutoff limit.
 val finalmaze2 = List(3,1,3,1,0,1,2,9).grouped(2).foldLeft(mazeOriginalWithVisitedInitialized)(shiftRowOrColumn)
                                                  //> finalmaze2  : List[List[(List[Char], Boolean)]] = List(List((List(0, 1, 1, 
                                                  //| 0),true), (List(1, 1, 0, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 1
                                                  //| , 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 0),false), (List(1, 0
                                                  //| , 0, 1),false), (List(0, 1, 0, 1),false), (List(1, 1, 0, 0),false), (List(0
                                                  //| , 1, 1, 1),false)), List((List(0, 1, 1, 0),false), (List(1, 0, 1, 0),false)
                                                  //| , (List(1, 1, 0, 0),false), (List(1, 0, 1, 0),false), (List(1, 0, 0, 1),fal
                                                  //| se), (List(1, 1, 1, 0),false), (List(1, 0, 1, 0),false), (List(1, 1, 0, 1),
                                                  //| false), (List(1, 1, 0, 0),false), (List(1, 0, 1, 0),false)), List((List(1, 
                                                  //| 1, 0, 0),false), (List(0, 0, 1, 1),false), (List(1, 1, 0, 0),false), (List(
                                                  //| 0, 1, 0, 1),false), (List(0, 1, 1, 1),false), (List(0, 1, 0, 1),false), (Li
                                                  //| st(1, 1, 1, 0),false), (List(1, 1, 0, 1),false), (List(1, 0, 0, 1),false), 
                                                  //| (List(0, 0, 1, 1),false)), List((List(0, 1, 0, 1),false), (List(1, 0, 1, 1)
                                                  //| ,false), (List(0, 1, 0,
                                                  //| Output exceeds cutoff limit.
 
 printSVGBoard(finalmaze, 6, m)
 printSVGBoard(finalmaze2, 7, m)

  
  
 val sol = Iterator.from(0).map( trial =>  {
 val bools = List.fill(numSteps)(scala.util.Random.nextInt(4))
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
  }).find( _._2 == 0).get                         //> Solution!!!
                                                  //| List(1, 8, 1, 3, 1, 1, 3, 2, 1, 9, 0, 4, 1, 2, 0, 1, 3, 1, 1, 4, 1, 2, 1, 
   /*                                             //| 4, 3, 1, 1, 9, 2, 7, 1, 1, 0, 8, 1, 8, 3, 9, 0, 2)
   ["D8", "R11", "D14", "R10", "R9", "D13", "L10", "D4", "D1", "D11", "D3", "R5", "R13", "D3", "D13", "L4", "D6", "L2", "U6", "L5", "U8", "D1", "D11", (2,0), "D6", "D8", "D2", "L4", "D1", (1,2), "R13", "D4", "R2", "D8", "D1", "U13", (2,1), "U12", "L12", "D3", "D4", "L3", "L14", "L8", (3,3), "R2", "D13", "D9", "L9", "U4", "R4", "L14", (8,6), "D7", (14,14)]


List(D8,R11,D14,R10, R9, D13, L10, D4,D1,D11,
D3, R5, R13, D3, D13, L4, D6, L2, U6, L5,U8,D1,D11,  // move 23
(2,0),
D6, D8, D2, L4, D1, // move 28
(1,2),
R13, D4, R2, D8,  // need to move the mouse before the next D1
D1,
U13,
(2,1) // 34
U12, L12,
D3, D4,

L3, L14, L8,  // 41
(3,3),
R2,  // this R2 seems kind of unnecessary 42
D13,D9, L9, U4, R4, L14,
(8,6),
D7, //49
, (14,14))
*/
                                                  //| Trial 0
                                                  //| 0
                                                  //| sol  : (List[Int], Int) = (List(1, 8, 1, 3, 1, 1, 3, 2, 1, 9, 0, 4, 1, 2, 
                                                  //| 0, 1, 3, 1, 1, 4, 1, 2, 1, 4, 3, 1, 1, 9, 2, 7, 1, 1, 0, 8, 1, 8, 3, 9, 0,
                                                  //|  2),0)
  
 println(sol._1)                                  //> List(1, 8, 1, 3, 1, 1, 3, 2, 1, 9, 0, 4, 1, 2, 0, 1, 3, 1, 1, 4, 1, 2, 1, 
                                                  //| 4, 3, 1, 1, 9, 2, 7, 1, 1, 0, 8, 1, 8, 3, 9, 0, 2)

 val printmazeList = sol._1.grouped(2).scanLeft(mazeOriginalWithVisitedInitialized)(shiftRowOrColumn)
                                                  //> printmazeList  : Iterator[List[List[(List[Char], Boolean)]]] = non-empty i
                                                  //| terator
 printmazeList.zipWithIndex.foreach{ x => printSVGBoard(x._1, x._2, m)}
   
}