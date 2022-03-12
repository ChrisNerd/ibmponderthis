import java.io._
import sys.process._
import scala.language.postfixOps
import scala.annotation.tailrec
object ponder {
 case class State ( board: Map[ ((Int,Int)), Int], robotPosition: (Int,Int), direction: (Int,Int),
  verticalWarps: Map[Int, (Int,Int)] = Map(), horizontalWarps: Map[Int, (Int,Int)] = Map() )
 def printState(state: State, N: Int) = {
  (0 until N).foreach{ y =>
   println( (0 until N).map{ x =>
   if (state.robotPosition == (x,y))
    "R"
   else
    state.board.getOrElse((x,y), 0 )
   }.mkString(" ") )
  }
  println("")
 }
 
 def move(state: State, N: Int): State = {
  // Step 1: Turn
  // Step 2: Dose
  // Step 3: Move
  //println("move " + state.board.size)
  val newDirection = state.board.get( state.robotPosition ) match {
   // If the cell was vaccinated 1 time, it administers one dose (raising the cell's status from "1" to "2") and turns 90 degrees COUNTERCLOCKWISE.
   case Some(doseNumber) if doseNumber == 1 => ( state.direction._2, -state.direction._1)
   // If the cell was vaccinated 2 times, the robot does not change direction.
   case Some(doseNumber) if doseNumber == 2 => state.direction
   // If the cell's status is "B", the robot avoids the anti-vaccine bots, administers no vaccine, and turns 90 degrees COUNTERCLOCKWISE.
   case Some(doseNumber) if doseNumber == 3 => ( state.direction._2, -state.direction._1)
   // If the cell was vaccinated 0 times, it administers one dose (raising the cell's status from "0" to "1") and turns 90 degrees CLOCKWISE.
   case None => ( -state.direction._2, state.direction._1)
  }
  val newPosition2 = ((state.robotPosition._1 + newDirection._1 + N) %N, (state.robotPosition._2 + newDirection._2 + N)%N)
    
  // So when N = 10, we want to go from -4 to +5. When N=11, we want to go from -5 to +5.
  val newPosition3 = ( if (newPosition2._1 > N/2) newPosition2._1 - N else newPosition2._1, if (newPosition2._2 > N/2) newPosition2._2 - N else newPosition2._2)
  
  // So, if we're at a warp point, update the newPosition.
  val newPosition = /* newPosition3 */ newDirection match {
   // going right, will see if current robotposition's x corrodinate matches the left side (._1) of the horizontal warp indexed by the robotposition's y (._2)
   // if so, keep the same y coordinate of the robot position, but update the new x coordinate to the right side (._2) of the warp value.
   case d if d == (1,0) => state.horizontalWarps.get(state.robotPosition._2) match {
     case Some(warpPair) if warpPair._1 == state.robotPosition._1 => (warpPair._2, state.robotPosition._2)
     case _ => newPosition3
     }
   case d if d == (-1,0) => state.horizontalWarps.get(state.robotPosition._2) match {
     case Some(warpPair) if warpPair._2 == state.robotPosition._1 => (warpPair._1, state.robotPosition._2)
     case _ => newPosition3
     }
     // This is going down
   case d if d == (0,1) => state.verticalWarps.get(state.robotPosition._1) match {
     case Some(warpPair) if warpPair._1 == state.robotPosition._2 => (state.robotPosition._1, warpPair._2)
     case _ => newPosition3
     }
   case d if d == (0,-1) => state.verticalWarps.get(state.robotPosition._1) match {
     case Some(warpPair) if warpPair._2 == state.robotPosition._2 => (state.robotPosition._1, warpPair._1)
     case _ => newPosition3
     }
  }
  
  val newBoard = state.board.get( state.robotPosition ) match {
  // If the cell was vaccinated 1 time, it administers one dose (raising the cell's status from "1" to "2") and turns 90 degrees COUNTERCLOCKWISE.
   case Some(doseNumber) if doseNumber == 1 => state.board.updated( state.robotPosition, 2)
   // If the cell was vaccinated 2 times, the robot does not change direction.
   case Some(doseNumber) if doseNumber >= 2 => state.board
   // If the cell was vaccinated 0 times, it administers one dose (raising the cell's status from "0" to "1") and turns 90 degrees CLOCKWISE.
   case None => state.board.updated( state.robotPosition, 1)
  }
  State( newBoard, newPosition, newDirection)
 }
 
 // Starting at state, iterate
/* def moveOrReturnFullPathIfStuck(state: State, pathSoFar: Vector[State] = Vector(), statesVisted: Set[State] = Set()): Vector[State] = {
 if (statesVisted.contains(state))
  pathSoFar
 else
  moveOrReturnFullPathIfStuck(move(state), pathSoFar :+ state, if(state.board.getOrElse(state.robotPosition, 0) < 2) Set() else
  statesVisted + state
  )
 }*/
 
 // Given a state, which includes the board, the robot position and direction.
 // Step 1. Determine if we're stuck. If so, return.
 // If not, move.
 def moveOrReturnFullPathIfStuck(
  N: Int,
  state: State,
  pathSoFar: Vector[State] = Vector(),
  potentiallyStuck: Boolean = false,
  firstStuckCell: (Int,Int) = (-1,-1),
  firstStuckDirection: (Int,Int) = (-1,-1)): Vector[State] = {
   //println("in move or return full path if stuck " + pathSoFar.size + " state size " + state.board.size + " robot " + state.robotPosition)
   if (potentiallyStuck && firstStuckCell == state.robotPosition && firstStuckDirection == state.direction && state.board.getOrElse(state.robotPosition, 0) == 2)
    pathSoFar :+ state
   else
   {
    moveOrReturnFullPathIfStuck(
     N,
     move(state, N),
     pathSoFar :+ state,
/*
     this is the problem
     Suppose, we are at a dose == 1.
     We are about to turn it into a dose == 2, then turn, then move, so we could be potentially the start of a loop. ie. potentiallyStuck.
     We need to record the direction after the turn, not before.
     So how about we only consider the first potentially stuck situation when we hit a dose==1 and record the direction after the dose.
*/
/*potentiallyStuck = */     state.board.getOrElse(state.robotPosition, 0) >= 1
     ,
/*firstStuckCell = */     if (state.board.getOrElse(state.robotPosition, 0) < 2)
      state.robotPosition
     else
/*      if (!potentiallyStuck)
       state.robotPosition
      else
  */     firstStuckCell
     ,
/*firstStuckDirection = */     if (state.board.getOrElse(state.robotPosition, 0) < 2)
         ( state.direction._2, -state.direction._1) //  move(state, N).direction
     else
/*      if (!potentiallyStuck)
       move(state, N).direction
      else
  */     firstStuckDirection
  )
  }
 }
 
 def moveUntilStuckBoolean(
  N: Int,
  state: State,
  allStatesSeenSoFar: Set[State] = Set(),
  potentiallyStuck: Boolean = false,
  firstStuckCell: (Int,Int) = (-1,-1),
  firstStuckDirection: (Int,Int) = (-1,-1)): Boolean = {
   //println("In stuck boolean. State " + state)
   //printState(state)
   if (allStatesSeenSoFar.contains(state) || potentiallyStuck && firstStuckCell == state.robotPosition && firstStuckDirection == state.direction && state.board.getOrElse(state.robotPosition, 0) == 2)
    state.board.values.forall( _ >= 2) && state.board.size == N*N // the check on the board size is to ensure there's no 0s left.
   else
   {
    moveUntilStuckBoolean(
     N,
     move(state, N),
     allStatesSeenSoFar + state,
     /*
     this is the problem
     Suppose, we are at a dose == 1.
     We are about to turn it into a dose == 2, then turn, then move, so we could be potentially the start of a loop. ie. potentiallyStuck.
     We need to record the direction after the turn, not before.
     So how about we only consider the first potentially stuck situation when we hit a dose==1 and record the direction after the dose.
*/
/*potentiallyStuck = */     state.board.getOrElse(state.robotPosition, 0) >= 1
     ,
/*firstStuckCell = */     if (state.board.getOrElse(state.robotPosition, 0) <= 1)
      state.robotPosition
     else
 /*     if (!potentiallyStuck)
       state.robotPosition
      else*/
       firstStuckCell
     ,
/*firstStuckDirection = */     if (state.board.getOrElse(state.robotPosition, 0) <= 1)
       ( state.direction._2, -state.direction._1) // move(state, N).direction
     else
/*      if (!potentiallyStuck)
       move(state, N).direction
      else*/
       firstStuckDirection
  )
  }
 }

 def moveUntilStuckPathOnly(
  N: Int,
  state: State,
  pathSoFar: Set[(Int,Int)] = Set(),
  potentiallyStuck: Boolean = false,
  firstStuckCell: (Int,Int) = (-1,-1),
  firstStuckDirection: (Int,Int) = (-1,-1)): Set[(Int,Int)] = {
   //printState(state,N)
   //println(pathSoFar.size)
   if (potentiallyStuck && firstStuckCell == state.robotPosition && firstStuckDirection == state.direction && state.board.getOrElse(state.robotPosition, 0) == 2)
    pathSoFar
   else
   {
    moveUntilStuckPathOnly(
     N,
     move(state, N),
     pathSoFar + state.robotPosition,
     /*
     this is the problem
     Suppose, we are at a dose == 1.
     We are about to turn it into a dose == 2, then turn, then move, so we could be potentially the start of a loop. ie. potentiallyStuck.
     We need to record the direction after the turn, not before.
     So how about we only consider the first potentially stuck situation when we hit a dose==1 and record the direction after the dose.
*/
/*potentiallyStuck = */     state.board.getOrElse(state.robotPosition, 0) >= 1
     ,
/*firstStuckCell = */     if (state.board.getOrElse(state.robotPosition, 0) == 1)
      state.robotPosition
     else
/*      if (!potentiallyStuck)
       state.robotPosition
      else
  */     firstStuckCell
     ,
/*firstStuckDirection = */     if (state.board.getOrElse(state.robotPosition, 0) == 1)
              ( state.direction._2, -state.direction._1) // move(state, N).direction
     else
   /*   if (!potentiallyStuck)
       move(state, N).direction
      else
     */  firstStuckDirection
  )  //+ state.robotPosition
  }
 }
 
 
 
  
 def doesSolve( state: State /*B1: (Int,Int), B2: (Int,Int)*/, N: Int) =
  moveUntilStuckBoolean(N, state)
  //moveUntilStuckBoolean(N, State(Map((B1, 3), (B2, 3)), (0,0), (0,-1)))
  //moveOrReturnFullPathIfStuck(N, State(Map((B1, 3), (B2, 3)), (0,0), (0,-1))).last.board.values.forall( _ >= 2)
 // At the beginning, the robot is located at the (0,0) cell facing UP. (UP is (0,-1) since the Y axis is down in the position direction
 
 val here1 = "here1"
 
 
 //Path0.foreach(printState)
                                                  
 
 
/* val solutions = for {
  B1 <- Path0.toIterator
  Path0minus = Path0.takeWhile( _ != B1) // drop(1) ???
  Path1 = moveOrReturnFullPathIfStuck(B1)
  B2 <- Path1.diff(Path0minus)
  if (doesSolve(board, B1,B2))
  } yield (B1,B2)
 */
 
 def printSVGBoard(state: State, number: Int, N: Int, lastkPos: Vector[(Int,Int)] = Vector()) = {
  val pw = new PrintWriter(new File("%07d".format(number) + ".svg" ))
  pw.write("<svg height=\"" + N + "\" width=\""+ N +"\">\n")
  (0 until N).foreach{ y =>
   (0 until N).map{ x =>
    pw.write("<circle cx=\"" + x + "\" cy=\"" + y + "\" r=\"0.4\" stroke=\"black\" stroke-width=\"0.03\" fill=\"")
    val currentCellColour =
    if (state.robotPosition == (x,y))
    "green"
   else
    state.board.get( (x,y) ) match {
    case Some(dose) if dose == 3 => "red"
    case Some(dose) if dose == 2 => "blue"
    case Some(dose) if dose == 1 => "yellow"
    case None => "grey"
   }
   pw.write(currentCellColour + "\" />\n")
  }
  pw.write("\n")
  }
  
  lastkPos.reverse.zipWithIndex.foreach{ case (v,i) =>
   pw.write("<circle cx=\"" + v._1 + "\" cy=\"" + v._2 + "\" r=\"0.4\" stroke=\"black\" stroke-width=\"0.03\" fill=\"green\" fill-opacity=\"" +
    { (lastkPos.size.toDouble - i)/lastkPos.size }.toString + "\"  />\n")
  }
  
  pw.write("</svg>")
  pw.close

  "gzip " + "%07d".format(number) + ".svg" !

  
 }
  
 def printAllSVGs(B1: (Int,Int), B2: (Int,Int), N: Int) = {
   val fullVectorOfState = moveOrReturnFullPathIfStuck(N, State(Map((B1, 3), (B2, 3)), (0,0), (0,-1)))
   val positionsOnly = fullVectorOfState.map(_.robotPosition)
   val k = 100
   
   // We really want this to use sliding, but sliding won't work for the first k elements.
   val vectorOfSlidingPositions = for{ pos <- 1 to positionsOnly.size } yield positionsOnly.take(pos).takeRight(k)
   fullVectorOfState.zip(vectorOfSlidingPositions)
   .zipWithIndex.foreach( s => printSVGBoard(s._1._1, s._2, N, s._1._2))
 
 }
 
 def main(args: Array[String]): Unit = {
 
 def Path0(N: Int) = moveOrReturnFullPathIfStuck(N, State( Map(), (0,0), (0,-1)))
 
// printAllSVGs((-1,-1), (-1,-1), 6)
 
/* def solutions(N: Int) = for {
  B1 <- //Path0(N).toStream
  Path0(N).toStream.filterNot{ var set = Set[(Int,Int)]()
    obj => val b = set(obj.robotPosition); set += obj.robotPosition; b}

  B2 <- {
println(B1)
moveOrReturnFullPathIfStuck(N, State(B1.board.updated(B1.robotPosition, 3), B1.robotPosition, B1.direction))
   .map(_.robotPosition).distinct.diff(Path0(N).map(_.robotPosition).takeWhile( _ != B1.robotPosition).distinct)} // there might need to be a drop(1) or dropWhile???
  if (doesSolve( B1.robotPosition, B2, N))
  // Hmmm, should be a way to start it as B2, but don't kn
//  moveOrReturnFullPathIfStuck(State(Map((B1, 3), (B2, 3)), (0,0), (0,-1))).last.board.values.forall( _ >= 2)
  
 } yield (B1,B2)
*/
 
 def intToNeg(N: Int)(in: Int) = if (in >=N/2) in - N else in
 
 def convertToNegativeIndex(N: Int)(inState: State ) = State(
  inState.board.map{ case (k,v) => ( (intToNeg(N)(k._1),intToNeg(N)(k._2)), v ) },
  (intToNeg(N)(inState.robotPosition._1),intToNeg(N)(inState.robotPosition._2)),
  (inState.direction._1,inState.direction._2)
 )

 def intToNonNeg(N: Int)(in: Int) = if (in < 0) in + N else in

 def convertToNonNegativeIndex(N: Int)(inState: State ) = State(
  inState.board.map{ case (k,v) => ( (intToNonNeg(N)(k._1),intToNonNeg(N)(k._2)), v ) },
  (intToNonNeg(N)(inState.robotPosition._1),intToNonNeg(N)(inState.robotPosition._2)),
  (inState.direction._1,inState.direction._2)
 )

 println("debug1")

 val fullpath50 = moveOrReturnFullPathIfStuck(50, State(Map((0,0) -> 3, (-1,0) -> 1, (-1,-1) -> 3),(-1,-1),(0,-1),Map(),Map()))
 println("fullpath50 size " + fullpath50.size)
 println("last " + fullpath50.last)
 
  
 println("debug2")

 def bs: Iterator[State] = {
  val maxBoard = 150
	println("inside bs 1")
  val p0 = Path0(maxBoard)//.map( convertToNegativeIndex(maxBoard) )
  
	println("inside bs 2")
	println("p0size " + p0.size)
	
  for {
   B1 <- {
    println("full length who cares " + p0.length + " distinct length " + p0.distinctBy(_.robotPosition).length)
    p0.distinctBy(_.robotPosition).takeWhile( x => ( x.robotPosition._1.abs + 5 )<maxBoard/2  && ( x.robotPosition._2.abs +5 )<maxBoard/2).iterator
   }
   B2 <- {
   // println("at B1 = " + B1)
    val path1 = moveOrReturnFullPathIfStuck(maxBoard, State(B1.board.updated(B1.robotPosition, 3), B1.robotPosition, B1.direction)).distinctBy(_.robotPosition)
    //println("inside bs 3")
    val path0minus = p0.map(_.robotPosition).takeWhile( _ != B1.robotPosition).toSet
   // println("inside bs 4 where's the path size???")
    println("path1 size " + path1.size + ". path0minus size " + path0minus.size)
    path1.filterNot(x => path0minus.contains(x.robotPosition) ).takeWhile( x => ( x.robotPosition._1.abs + 2 )<maxBoard/2  && ( x.robotPosition._2.abs +2 )<maxBoard/2)
   }
  } yield {
  //println("yielding b2 " + B2 + " b1 " + B1)
  val m = moveOrReturnFullPathIfStuck(maxBoard, State(B2.board.updated(B2.robotPosition, 3), B2.robotPosition, B2.direction))
	//println("m " + m.size)
	//println("m head " + m.head)
	//println("last 3 m " + m.takeRight(3))
  val retval = m.find( s =>
  { //println(s.robotPosition) // lets see if taking out the warpming fixes this
  
  (s.robotPosition._1.abs + 2 >= maxBoard/2) || (s.robotPosition._2.abs + 2 >= maxBoard/2)}).get
  //println("retval " + retval)
  retval
  }
 }

 println("here4")
// bs.take(3).foreach(x => println("printing take 4 " + x))
 println("here5")

 def createWarpPairs(inState: State, N: Int): State = State(
 inState.board,
 inState.robotPosition,
 inState.direction,
 (for{
   v <- -N/2 to N/2
   x1 =  1 + Iterator.from(0, -1).find( h => inState.board.getOrElse( (h,v), 0) != 2).get
   x2 = -1 + Iterator.from(0, +1).find( h => inState.board.getOrElse( (h,v), 0) != 2).get
   if (x1 < x2)
   }
   yield (v ,(x1,x2) )).toMap,
 (for{
   h <- -N/2 to N/2
   y1 =  1 + Iterator.from(0, -1).find( v => inState.board.getOrElse( (h,v), 0) != 2).get
   y2 = -1 + Iterator.from(0, +1).find( v => inState.board.getOrElse( (h,v), 0) != 2).get
   if (y1 < y2)
   }
   yield (h ,(y1,y2) )).toMap)
 
 @tailrec def solve( unsolvedNs: Set[Int], state: Iterator[State]): Unit =
  if (!unsolvedNs.isEmpty && state.hasNext )
  {
    val s = state.next
    val (solvedByState, notSolvedByState) = unsolvedNs.partition( N => doesSolve( createWarpPairs(s, N) /*convertToNonNegativeIndex(N)(s)*/, N))
    solvedByState.foreach( N => println("State " + s.board.filter( (t) => t._2 == 3 ).keySet + " solves " + N))
    println("not solved " + notSolvedByState.size + " statesize " + s.board.size)
    solve( notSolvedByState, state)
  }
  
 println("hereChris1")

 //solve((150 to 175).toSet, bs)
  //solve((36 to 60).toSet, bs)
 println("hereChris2")

 def solutions(N: Int) =
 {
 val p0 = Path0(N)
 for {
  B1 <- //Path0(N).toStream
  {
  //println("Solutions for n= " + N + " Full length " + p0.length + " distinct length " + p0.distinctBy(_.robotPosition).length)
  p0.distinctBy(_.robotPosition).to(LazyList)/*filterNot{ var set = Set[(Int,Int)]()
    //obj => val b = set(obj.robotPosition); set += obj.robotPosition; b}*/
}
  B2 <- {
   //println(B1)
   val path1 = moveOrReturnFullPathIfStuck(N, State(B1.board.updated(B1.robotPosition, 3), B1.robotPosition, B1.direction)).distinctBy(_.robotPosition)
   val path0minus = p0.map(_.robotPosition).takeWhile( _ != B1.robotPosition).toSet
 //  println("path1 size " + path1.size + ". path0minus size " + path0minus.size)
   path1.filterNot(x => path0minus.contains(x.robotPosition) )
   } // there might need to be a drop(1) or dropWhile???
  if (doesSolve( State(B2.board.updated( B2.robotPosition, 3), B2.robotPosition, B2.direction), N))
  // Hmmm, should be a way to start it as B2, but don't kn
//  moveOrReturnFullPathIfStuck(State(Map((B1, 3), (B2, 3)), (0,0), (0,-1))).last.board.values.forall( _ >= 2)
  
 } yield (B1.robotPosition,B2.robotPosition)
}
 //solutions(5).take(2).foreach(println)
 /*
 val allSolutions = List(
((0,0),(5,5)),
((0,0),(1,0)),
((0,0),(1,0)),
((0,0),(0,7)),
((0,0),(5,0)),
((0,0),(0,10)),
((0,0),(1,1)),
((1,0),(11,9)),
((1,0),(13,1)),
((0,0),(3,0)),
((0,0),(0,14)),
((0,0),(13,0)),
((1,0),(16,16)),
((0,0),(0,16)),
((0,0),(19,1)),
((0,0),(2,1)),
((1,0),(0,6)),
((0,0),(3,1)),
((1,1),(6,1)),
((1,1),(4,5)),
((1,1),(4,2)),
((1,0),(0,10)),
((1,1),(27,23)),
((1,0),(2,2)),
((1,1),(8,2)),
((1,1),(29,4)),
((1,0),(2,1)),
((0,0),(28,0)),
((1,0),(2,7)),
((0,0),(0,32)),
((1,1),(8,35)),
((0,0),(0,32)),
((1,1),(15,33)),
((0,0),(0,13)),
((1,1),(11,15)),
((1,1),(40,39)),
((1,1),(14,35)),
((42,42),(4,42)),
((43,1),(11,9)),
((1,1),(38,28)),
((1,1),(42,44)),
((0,46),(0,24)),
((0,47),(12,47)),
((0,0),(0,3)),
((0,49),(46,3)),
((1,1),(0,6)),
((1,1),(7,41)),
((52,1),(43,47)),
((0,0),(0,15)),
((1,1),(0,21)),
((1,1),(1,12)),
((0,56),(0,41)),
((1,0),(1,55)),
((1,1),(31,18)),
((1,1),(17,37)),
((0,60),(0,37)),
((1,1),(4,52)),
((62,1),(1,55)),
((63,63),(61,3)),
((62,0),(14,6)),
((0,65),(8,65)),
((66,1),(22,10)),
((66,67),(63,59)),
((1,1),(9,61)),
((1,1),(1,16)),
((70,1),(53,63)),
((1,1),(62,64)),
((70,0),(0,37)),
((72,73),(61,53)),
((1,1),(64,51)),
((0,2),(4,20)),
((2,1),(0,31)),
((77,1),(1,70)),
((0,76),(35,21)),
((79,1),(18,31)),
((1,1),(71,77)),
((81,1),(80,10)),
((0,2),(39,2)),
((83,83),(79,83)),
((82,0),(15,77)),
((83,1),(0,25)),
((85,86),(17,32)),
((0,2),(86,65)),
((87,88),(9,29)),
((87,0),(34,24)),
((1,1),(4,66)),
((90,91),(46,15)),
((92,1),(31,10)),
((91,0),(0,48)),
((1,1),(94,36)),
((93,0),(12,9)),
((0,2),(88,22)),
((95,0),(65,89)),
((1,1),(4,92)),
((1,97),(8,2)),
((1,1),(82,36)),
((0,101),(49,101)),
((0,99),(55,99)),
((102,103),(79,62)),
((103,104),(15,76)),
((104,105),(81,27)),
((0,2),(50,2)),
((0,105),(0,59)),
((107,108),(98,40)),
((1,1),(39,75)),
((0,107),(0,48)),
((1,1),(57,75)),
((0,2),(60,25)),
((1,112),(5,21)),
((1,112),(23,102)),
((114,114),(18,7)),
((115,116),(51,37)),
((0,4),(59,116)),
((0,116),(40,116)),
((0,2),(58,2)),
((0,118),(0,51)),
((1,2),(0,60)),
((3,1),(113,93)),
((123,1),(38,117)),
((2,2),(16,104)),
((124,125),(112,10)),
((125,126),(0,59)),
((126,127),(28,106)),
((1,127),(116,114)),
((0,4),(65,128)),
((130,127),(0,87)),
((1,1),(0,63)),
((127,131),(132,51)),
((0,4),(67,132)),
((1,1),(49,102)),
((1,133),(57,47)),
((1,1),(35,46)),
((0,4),(69,136)),
((138,4),(0,67)),
((0,4),(70,138)),
((0,135),(64,135)),
((0,4),(71,140)),
((140,137),(85,111)),
((1,142),(61,31)),
((0,142),(0,84)),
((1,144),(0,51)),
((146,4),(0,24)),
((3,147),(92,121)),
((0,146),(63,146)),
((0,4),(75,148)),
((149,150),(18,33)),
((0,4),(76,150)),
((0,150),(0,84)),
((1,4),(7,10)),
((152,149),(79,119)))
 
 val solsWithN = allSolutions.zip((6 to 155))
 
 for ( s <- solsWithN ) println( s + {if (doesSolve( State(Map( (s._1._1, 3), (s._1._2, 3)), (0,0), (0,-1)), s._2)) "true" else "false" } )
 
 val sDoesntSolve = (((4,5),(6,7)), 25)
 println( sDoesntSolve + {if (doesSolve( State(Map( (sDoesntSolve._1._1, 3), (sDoesntSolve._1._2, 3)), (0,0), (0,-1)), sDoesntSolve._2)) "true" else "false" } )
 
 println("here1")
 val s50 = solutions(50)
 
 */
/*
  s50.foreach(x => println( "Solution " + x))
 println("here2")
 
 
  println("number of Solutions found = " + s50.size) // grep -c reveals it to be 75
  
 println("here3")
 */

 // printAllSVGs(s50.head._1.robotPosition, s50.head._2.robotPosition, 50)
 
 val needed = Set(80,81,83,84,85,86,89,90,91,93,96,98,103,104,106,109,111,115,117,118,119,121,123,125,126,127,128,129,130,133,134,137,138,139,
 140,141,142,143,145,148,149,150,151,152,154)
 
 
 Iterator.from(6).foreach{ N =>
  println( N + " has " + solutions(N).size + " solutions")}
 
 
 /*
 Iterator.from(83,4).filter(x => x > needed.max || needed.contains(x)).find{ N => {
  println("N = " + N)
  solutions(N) match {
   case LazyList() =>
   {
    println("No Solution for N = " + N)
    true
   }
   case h #:: _ =>
   {
    println("Solution " + h)
    false
    }
  }
 }
 }*/
 
 
 
 
/*
 "rm *.svg *.png" !
 
 printAllSVGs( (1,97),(8,2)) // Shows the solution for N=100
 printAllSVGs(solutions.head._1.robotPosition, solutions.head._2)
  
// "for i in *.svg ; do convert \"$i\" \"${i%.*}.png\" ; done" !
  
  
// "ffmpeg -r 60 -i %07d.png -c:v libx264 -pix_fmt yuv420p out.mp4" !
 for f in *.svg.gz; do mv -- "$f" "$(basename -- "$f" .svg.gz).svg"; done
 
 "ffmpeg -width 640 -r 60 -i %07d.svg -c:v libx264 -pix_fmt yuv420p outsvg100.mp4" !
  
 "mplayer -fs outsvg100.mp4" !
 */
 }
}